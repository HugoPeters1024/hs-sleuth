{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HsComprehension.Plugin where

import Prelude as P
import Data.Maybe
#if MIN_VERSION_ghc(9,0,0)
import GHC.Plugins
#else
import GhcPlugins
#endif

import HsComprehension.Uniqify as Uniqify
import HsComprehension.Ast as Ast
import qualified HsComprehension.Cvt as Cvt
import HsComprehension.DefAnalysis

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
import Data.Traversable (for)

import Text.Parsec as Parsec
import Text.Parsec.Number as Parsec

import Data.ByteString.Lazy (hPutStr)
import qualified GhcDump.Convert

import Data.Time
import Data.Time.Clock
import Data.Time.Clock.POSIX

#if MIN_VERSION_ghc(9,0,0)
import qualified GHC.Data.EnumSet (insert)
#endif

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
                 <*> pure (p+1)

parseStdout :: String -> [Ast.FiredRule]
parseStdout inp = reverse $ fst $ P.foldl go ([], 0) (lines inp)
    where go :: ([Ast.FiredRule], Int) -> String -> ([Ast.FiredRule], Int)
          go (acc, p) s =
              case eitherToMaybe (Parsec.runParser phaseMarkerParser () "stdout" s) of
                Just np -> (acc, np)
                Nothing -> case eitherToMaybe (Parsec.runParser (ruleParser p) () "stdout" s) of
                    Just x -> (x:acc, p)
                    Nothing -> (acc, p)


data CaptureView = CaptureView
  { cv_project_root :: FilePath
  }

defaultCaptureView :: CaptureView
defaultCaptureView = CaptureView
  { cv_project_root = "./"
  }


currentPosixMillis :: IO Int
currentPosixMillis = 
  let posix_time =
#if MIN_VERSION_ghc(9,0,0)
        getPOSIXTime
#else
        utcTimeToPOSIXSeconds <$> getCurrentTime
#endif
  in floor . (1e3 *) . toRational <$> posix_time

cvtGhcPhase :: DynFlags -> Int -> String -> ModGuts -> Ast.Phase
cvtGhcPhase dflags phaseId phase =
    let cvtEnv = Cvt.CvtEnv { Cvt.cvtEnvPhaseId = phaseId
                            , Cvt.cvtEnvBinders = []
                            }
    in Cvt.cvtPhase cvtEnv . GhcDump.Convert.cvtModule dflags phaseId phase

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
plugin = defaultPlugin 
  { installCoreToDos = install
#if MIN_VERSION_ghc(9,2,0)
  , driverPlugin = modifyDynFlags 
#elif MIN_VERSION_ghc(9,0,0)
  , dynflagsPlugin = modifyDynFlags
#endif
  }

#if MIN_VERSION_ghc(9,2,0)
modifyDynFlags :: [CommandLineOption] -> HscEnv -> IO HscEnv
modifyDynFlags _ hsc_env = 
  let updateFlags dflags = dflags { dumpFlags = GHC.Data.EnumSet.insert Opt_D_dump_rule_firings (dumpFlags dflags) }
  in pure $ hsc_env { hsc_dflags = updateFlags (hsc_dflags hsc_env) }
#elif MIN_VERSION_ghc(9,0,0)
modifyDynFlags :: [CommandLineOption] -> DynFlags -> IO DynFlags
modifyDynFlags _ dflags = 
  let updateFlags dflags = dflags { dumpFlags = GHC.Data.EnumSet.insert Opt_D_dump_rule_firings (dumpFlags dflags) }
  in pure (updateFlags dflags)
#endif

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install options todo = do
#if MIN_VERSION_ghc(9,0,0)
#else
    liftIO $ putStrLn "HsComprehension: GHC < 9.0.0 requires manual enabling of -ddump-rule-firings to get complete telemetry"
#endif
    liftIO setupProjectStdoutThief
    let slug = case options of
                    [slug] -> slug
                    _      -> error "provide a slug for the dump as exactly 1 argument"
    liftIO $ print options
    liftIO $ FP.createDirectoryIfMissing True (coreDumpDir defaultCaptureView slug)
    dflags <- getDynFlags
    modName <- showSDoc dflags . ppr <$> getModule
    liftIO $ modifyIORef projectState $ \(thief, capture) ->
        ( thief
        , capture
            { captureModules = (T.pack modName, length todo) : captureModules capture
            , captureName = T.pack slug
            }
        )

    ms_ref <- liftIO $ newIORef []
    let dumpPasses = zipWith (dumpPass ms_ref) [1..] (map getPhase todo)
    let firstPass = dumpPass ms_ref 0 "Desugared"
    pure $ firstPass : (P.concat $ zipWith (\x y -> [x,y]) todo dumpPasses) ++ [finalPass ms_ref (slug, modName)]

getPhase :: CoreToDo -> String
getPhase todo = showSDocUnsafe (ppr todo) ++ " " ++ showSDocUnsafe (pprPassDetails todo)

printPpr :: (Outputable a, MonadIO m) => a -> m ()
printPpr a = liftIO $ putStrLn $ showSDocUnsafe (ppr a)

coreDumpBaseDir :: CaptureView -> String
coreDumpBaseDir view = cv_project_root view ++ "dist-newstyle/"

coreDumpDir :: CaptureView -> String -> FilePath
coreDumpDir view pid = coreDumpBaseDir view ++ "coredump-" ++ pid ++ "/"

coreDumpFile :: CaptureView -> String -> String -> FilePath
coreDumpFile view pid mod = coreDumpDir view pid ++ mod ++ ".zstd"

captureFile :: CaptureView -> String -> FilePath
captureFile view pid = coreDumpDir view pid ++ "capture.zstd"

writeToFile :: (Serialise a) => FilePath -> a -> IO ()
writeToFile fname = do
    BSL.writeFile fname . Zstd.compress 2 . Ser.serialise

readFromFile :: Serialise a => FilePath -> IO a
readFromFile fname = do
    Ser.deserialise . Zstd.decompress <$> BSL.readFile fname

dumpPass :: IORef [Ast.Phase] -> Int -> String -> CoreToDo
dumpPass ms_ref n phase = CoreDoPluginPass "Core Snapshot" $ \in_guts -> do
    let guts = in_guts { mg_binds = Uniqify.freshenUniques (mg_binds in_guts) }
--    guts <- liftIO $ pure in_guts

    dflags <- getDynFlags
    let prefix :: String = showSDocUnsafe (ppr (mg_module guts))
    liftIO $ do
        putStrLn $ "__PHASE_MARKER " ++ show n
        let mod = cvtGhcPhase dflags n phase guts
        modifyIORef ms_ref (mod:)
    pure guts

finalPass :: IORef [Ast.Phase] -> (String, String) -> CoreToDo
finalPass ms_ref (slug, modName) = CoreDoPluginPass "Finalize Snapshots" $ \guts -> do
    liftIO $ do
        (thief, capture) <- readIORef projectState
        in_phases <- readIORef ms_ref
        r <- readStdoutThief thief
        let ruleFirings = parseStdout r
        putStrLn r

        let phases = defAnalysis $ map (\(n, p) -> p { phaseFiredRules = filter ((==n) . firedRulePhase) ruleFirings }) $ zip [0..] (reverse in_phases)

        let mod = Ast.Module {
              Ast.moduleName = T.pack modName
            , Ast.modulePhases = phases
            }

        let fname = coreDumpFile defaultCaptureView slug modName
        writeToFile fname mod

        writeToFile (captureFile defaultCaptureView (T.unpack (captureName capture))) capture
    pure guts
