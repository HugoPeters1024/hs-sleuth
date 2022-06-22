{-# LANGUAGE ScopedTypeVariables #-}
module HsComprehension.Plugin where

import Prelude as P
import Data.Maybe
import GHC.Plugins

import HsComprehension.Uniqify as Uniqify
import HsComprehension.Ast as Ast
import qualified HsComprehension.Cvt as Cvt

import Control.Monad.IO.Class
import Control.Monad
import qualified Data.ByteString.Lazy as BSL
import Codec.Serialise (Serialise)

import qualified Codec.Serialise as Ser
import qualified Codec.Compression.Zstd.Lazy as Zstd

import System.FilePath.Posix as FP
import System.Directory as FP
import System.IO (openFile, openTempFile, IOMode(..), stdout)
import System.IO.Unsafe (unsafePerformIO)
import GHC.IO.Handle
import Data.IORef

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.List (isPrefixOf, splitAt)

import Text.Parsec as Parsec
import Text.Parsec.Number as Parsec

import Data.ByteString.Lazy (hPutStr)
import qualified GhcDump.Convert

import Data.Time
import Data.Time.Clock.POSIX

type StdThief = (FilePath, Handle)

setupStdoutThief :: IO StdThief
setupStdoutThief = do
  tmpd <- getTemporaryDirectory
  (tmpf, tmph) <- openTempFile tmpd "haskell_stdout"
  stdout_dup <- hDuplicate stdout
  hDuplicateTo tmph stdout
  hClose tmph
  pure (tmpf, stdout_dup)

readStdoutThief :: StdThief -> IO String
readStdoutThief (tmpf, stdout_dup) = do
  hDuplicateTo stdout_dup stdout
  str <- readFile tmpf
  removeFile tmpf
  return str

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right x) = Just x

phaseMarkerParser :: Parsec.Parsec String () Int
phaseMarkerParser = id <$ Parsec.string "__PHASE_MARKER " <*> Parsec.int

ruleParser :: Int -> Parsec.Parsec String () Ast.FiredRule
ruleParser p = Ast.FiredRule 
                 <$ Parsec.string "Rule fired: "
                 <*> (T.pack <$> Parsec.manyTill Parsec.anyChar (Parsec.try (Parsec.string " (")))
                 <*> (T.pack <$> Parsec.manyTill Parsec.anyChar (Parsec.try (Parsec.char ')')))
                 <*> (pure p)

parseStdout :: String -> [Ast.FiredRule]
parseStdout inp = reverse $ fst $ P.foldl go ([], 0) (lines inp)
    where go :: ([Ast.FiredRule], Int) -> String -> ([Ast.FiredRule], Int)
          go (acc, p) s =
              case eitherToMaybe (Parsec.runParser phaseMarkerParser () "stdout" s) of
                Just np -> (acc, np)
                Nothing -> case eitherToMaybe (Parsec.runParser (ruleParser p) () "stdout" s) of
                    Just x -> (x:acc, p)
                    Nothing -> (acc, p)



millisSinceEpoch :: POSIXTime -> Int
millisSinceEpoch =
    floor . (1e3 *) . nominalDiffTimeToSeconds

currentPosixMillis :: IO Int
currentPosixMillis = millisSinceEpoch <$> getPOSIXTime

cvtGhcModule :: DynFlags -> Int -> String -> GHC.Plugins.ModGuts -> Ast.Module
cvtGhcModule dflags phaseId phase = 
    let cvtEnv = Cvt.CvtEnv { Cvt.cvtEnvPhaseId = phaseId
                            , Cvt.cvtEnvBinders = []
                            }
    in Cvt.cvtModule cvtEnv . GhcDump.Convert.cvtModule dflags phaseId phase

projectState :: IORef (StdThief, Capture)
projectState =  do
    let capture = 
            Capture { captureName = T.empty
                    , captureDate = 0
                    , captureModules = []
                    }
    unsafePerformIO $ do
        time <- currentPosixMillis
        newIORef (undefined, capture { captureDate = time })

setupProjectStdoutThief :: IO ()
setupProjectStdoutThief = do
    thief <- setupStdoutThief
    modifyIORef projectState $ \(_, capture) -> (thief, capture)

plugin :: Plugin
plugin = defaultPlugin { installCoreToDos = install }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install options todo = do
    liftIO setupProjectStdoutThief
    let slug = case options of
                    [slug] -> slug
                    _      -> error "provide a slug for the dump as exactly 1 argument"
    liftIO $ print options
    liftIO $ FP.createDirectoryIfMissing True (coreDumpDir slug)
    modName <- showPprUnsafe <$> getModule
    liftIO $ modifyIORef projectState $ \(thief, capture) -> 
        ( thief
        , capture 
            { captureModules = (T.pack modName, length todo) : captureModules capture 
            , captureName = T.pack slug
            }
        )

    ms_ref <- liftIO $ newIORef []
    let dumpPasses = zipWith (dumpPass ms_ref slug) [1..] (map getPhase todo)
    let firstPass = dumpPass ms_ref slug 0 "Desugared"
    pure $ firstPass : (P.concat $ zipWith (\x y -> [x,y]) todo dumpPasses) ++ [finalPass ms_ref]

getPhase :: CoreToDo -> String
getPhase todo = showSDocUnsafe (ppr todo) ++ " " ++ showSDocUnsafe (pprPassDetails todo)

printPpr :: (Outputable a, MonadIO m) => a -> m ()
printPpr a = liftIO $ putStrLn $ showSDocUnsafe (ppr a)

coreDumpBaseDir :: String
coreDumpBaseDir = "./dist-newstyle/"

coreDumpDir :: String -> FilePath
coreDumpDir pid = coreDumpBaseDir ++ "coredump-" ++ pid ++ "/"

coreDumpFile :: String -> String -> Int -> FilePath
coreDumpFile pid mod id = coreDumpDir pid ++ mod ++ "." ++ show id ++ ".zstd"

captureFile :: String -> FilePath
captureFile pid = coreDumpDir pid ++ "capture.zstd"

writeToFile :: (Serialise a) => FilePath -> a -> IO ()
writeToFile fname = do
    BSL.writeFile fname . Zstd.compress 2 . Ser.serialise

readFromFile :: Serialise a => FilePath -> IO a
readFromFile fname = do
    Ser.deserialise . Zstd.decompress <$> BSL.readFile fname

dumpPass :: IORef [(String, Ast.Module)] -> String -> Int -> String -> CoreToDo
dumpPass ms_ref slug n phase = CoreDoPluginPass "Core Snapshot" $ \in_guts -> do
    guts <- liftIO $ Uniqify.uniqueModule in_guts

    dflags <- getDynFlags
    let prefix :: String = showSDocUnsafe (ppr (mg_module guts))
    let fname = coreDumpFile slug prefix n
    liftIO $ do
        putStrLn $ "__PHASE_MARKER " ++ show n
        putStrLn fname
        let mod = cvtGhcModule dflags n phase guts
        modifyIORef ms_ref ((fname, mod):)
    pure guts

finalPass :: IORef [(String, Ast.Module)] -> CoreToDo
finalPass ms_ref = CoreDoPluginPass "Finalize dump" $ \guts -> do
    liftIO $ do
        (thief, capture) <- readIORef projectState 
        in_modules <- readIORef ms_ref

        r <- readStdoutThief thief
        let ruleFirings = parseStdout r
        forM_ (zip [0..] (reverse in_modules)) $ \(n, (fname, in_module)) -> do
            let mod = in_module { moduleFiredRules = filter ((==n) . firedRulePhase) ruleFirings }
            putStrLn fname
            writeToFile fname mod

        putStrLn r
        writeToFile (captureFile (T.unpack (captureName capture))) capture
    pure guts
