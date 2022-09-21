{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HsComprehension.Plugin where

import Prelude as P
import Data.Maybe


import GHC
# if MIN_VERSION_ghc(9,0,0)
import GHC.Plugins as Plugins
# else
import GhcPlugins as Plugins
# endif

#if MIN_VERSION_ghc(9,0,0)
import qualified GHC.Data.EnumSet (insert)
#endif


import HsComprehension.Uniqify as Uniqify
import HsComprehension.Ast as Ast
import qualified HsComprehension.Cvt as Cvt
import HsComprehension.DefAnalysis
import HsComprehension.ElmDeriving

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
import Data.Char (isSpace)

import Text.Parsec as Parsec
import Text.Parsec.Number as Parsec

import Data.ByteString.Lazy (hPutStr)
import Data.Aeson (encode, ToJSON)
import qualified GhcDump.Convert

import Data.Time
import Data.Time.Clock
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
phaseMarkerParser = do
  Parsec.string "__PHASE_MARKER "
  n <- Parsec.int
  Parsec.newline
  pure n

anyLine :: Parsec.Parsec String () String
anyLine = manyTill Parsec.anyChar Parsec.newline

ruleParser :: Int -> Parsec.Parsec String () Ast.FiredRule
ruleParser p = do
   Parsec.string "Rule fired:"
   Parsec.skipMany (Parsec.satisfy isSpace)
   chuncks <- words <$> anyLine
   let rulename = unwords (init chuncks)
   let modname = tail (init (last chuncks))
   pure $ Ast.FiredRule (T.pack rulename) (T.pack modname) (p+1)

stdoutParser :: Parsec.Parsec String () [Ast.FiredRule]
stdoutParser = 
  let phaseParser = (phaseMarkerParser >>= \n -> Parsec.many (ruleParser n)) <|> (anyLine >> phaseParser)
  in concat <$> many phaseParser

parseStdout :: String -> [Ast.FiredRule]
parseStdout inp = case Parsec.runParser stdoutParser () "stdout" inp of
  Right rs -> rs
  Left e -> error $ "Failed to parse rewrite rule output from stdout: " ++ show e

  


data CaptureView = CaptureView
  { cv_project_root :: FilePath
  , cv_direct_path :: Maybe FilePath
  }

defaultCaptureView :: CaptureView
defaultCaptureView = CaptureView
  { cv_project_root = "./"
  , cv_direct_path = Nothing
  }


currentPosixMillis :: IO Int
currentPosixMillis =
  let posix_time = utcTimeToPOSIXSeconds <$> getCurrentTime
  in floor . (1e3 *) . toRational <$> posix_time

cvtGhcPhase :: DynFlags -> Int -> String -> ModGuts -> Ast.Phase
cvtGhcPhase dflags phaseId phase =
    let cvtEnv = Cvt.CvtEnv { Cvt.cvtEnvPhaseId = phaseId
                            , Cvt.cvtEnvBinders = []
                            }
    in Cvt.cvtPhase cvtEnv . GhcDump.Convert.cvtModule dflags phaseId phase

projectState :: IORef (Bool, StdThief, Capture)
projectState =  do
    let capture =
            Capture { captureName = T.empty
                    , captureDate = 0
                    , captureGhcVersion = T.pack "GHC version unknown"
                    , captureModules = []
                    }
    unsafePerformIO $ do
        time <- currentPosixMillis
        newIORef (False, undefined, capture { captureDate = time })

setupProjectStdoutThief :: IO ()
setupProjectStdoutThief = do
    thief <- setupStdoutThief
    modifyIORef projectState $ \(reset, _, capture) -> (reset, thief, capture)

plugin :: Plugin
plugin = defaultPlugin 
  { installCoreToDos = install
#if MIN_VERSION_ghc(9,2,0)
  , driverPlugin = modifyDynFlags 
#elif MIN_VERSION_ghc(9,0,0)
  , dynflagsPlugin = modifyDynFlags
#endif
  , parsedResultAction = parsedPlugin
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

getGhcVersionString :: CoreM String
getGhcVersionString = do
  ghc_v <- ghcNameVersion <$> getDynFlags
  pure $ ghcNameVersion_programName ghc_v ++ " " ++ ghcNameVersion_projectVersion ghc_v

parseCmdLineOptions :: [CommandLineOption] -> String
parseCmdLineOptions options = case options of
    [slug] -> slug
    _      -> error "provide a slug for the dump as exactly 1 argument"

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install options todo = do

    dflags <- getDynFlags
    modName <- Plugins.moduleNameString . Plugins.moduleName <$> getModule
    ghc_version <- getGhcVersionString

    let ghc_version_major :: Int = read $ head $ split '.' $ drop 4 ghc_version

    when (ghc_version_major < 9) $ do
      liftIO $ putStrLn "HsComprehension: GHC < 9.0.0 requires manual enabling of -ddump-rule-firings to get complete telemetry"

    let slug = parseCmdLineOptions options
    liftIO setupProjectStdoutThief
    liftIO $ FP.createDirectoryIfMissing True (coreDumpDir defaultCaptureView slug)
    liftIO $ modifyIORef projectState $ \(setup, thief, capture) ->
        ( setup
        , thief
        , capture
            { captureModules = (T.pack modName, length todo) : captureModules capture
            , captureGhcVersion = T.pack ghc_version
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
coreDumpBaseDir view = case cv_direct_path view of
  Nothing -> cv_project_root view `FP.combine` "dist-newstyle"
  Just fp -> fp

coreDumpDir :: CaptureView -> String -> FilePath
coreDumpDir view pid = coreDumpBaseDir view `FP.combine` "coredump-" ++ pid

coreDumpFile :: CaptureView -> String -> String -> FilePath
coreDumpFile view pid mod = coreDumpDir view pid `FP.combine` mod ++ ".json"

captureFile :: CaptureView -> String -> FilePath
captureFile view pid = coreDumpDir view pid `FP.combine` "capture.json"

writeToFile :: (ToJSON a) => FilePath -> a -> IO ()
writeToFile fname = do
    BSL.writeFile fname . encode

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
        (_, thief, capture) <- readIORef projectState
        in_phases <- readIORef ms_ref
        r <- readStdoutThief thief
        let ruleFirings = parseStdout r
        putStrLn r

        let phases = defAnalysis $ zipWith (\ n p
              -> p {phaseFiredRules = filter
                                        ((== n) . firedRulePhase) ruleFirings}) [0..] (reverse in_phases)

        let mod = Ast.Module {
              Ast.moduleName = T.pack modName
            , Ast.modulePhases = phases
            }

        let fname = coreDumpFile defaultCaptureView slug modName
        writeToFile fname mod

        writeToFile (captureFile defaultCaptureView (T.unpack (captureName capture))) capture
    pure guts

parsedPlugin :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
parsedPlugin options modsum parsed = do
  let slug = parseCmdLineOptions options
  liftIO $ FP.createDirectoryIfMissing True (coreDumpDir defaultCaptureView slug)
  case ml_hs_file (ms_location modsum) of
    Just loc -> do
      let src_loc = cv_project_root defaultCaptureView `FP.combine` loc
      exists <- liftIO $ FP.doesFileExist src_loc
      when exists $ do
        let mod_name = Plugins.moduleNameString (Plugins.moduleName (ms_mod modsum))
        let dst_loc = coreDumpDir defaultCaptureView slug `FP.combine` (mod_name ++ ".hs")
        liftIO $ putStrLn $ "copying " ++ src_loc ++ " to " ++ dst_loc
        liftIO $ FP.copyFile src_loc dst_loc
    Nothing -> pure ()
  pure parsed

