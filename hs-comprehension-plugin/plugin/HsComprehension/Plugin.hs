{-# LANGUAGE ScopedTypeVariables #-}
module HsComprehension.Plugin where

import Prelude as P
import Data.Maybe
import GHC.Plugins

import HsComprehension.Uniqify as Uniqify
import HsComprehension.Ast
import HsComprehension.Cvt
import qualified HsComprehension.Embellish as Embellish

import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as BSL
import Codec.Serialise (Serialise)

import qualified Codec.Serialise as Ser
import qualified Codec.Compression.Zstd.Lazy as Zstd

import System.FilePath.Posix as FP
import System.Directory as FP
import System.IO (openFile, IOMode(..))
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import Data.ByteString.Lazy (hPutStr)
import qualified GhcDump.Convert

import Data.Time
import Data.Time.Clock.POSIX

millisSinceEpoch :: POSIXTime -> Int
millisSinceEpoch =
    floor . (1e3 *) . nominalDiffTimeToSeconds

currentPosixMillis :: IO Int
currentPosixMillis = millisSinceEpoch <$> getPOSIXTime

cvtGhcModule :: DynFlags -> Int -> String -> GHC.Plugins.ModGuts -> HsComprehension.Ast.Module
cvtGhcModule dflags phaseId phase = 
    let embEnv = Embellish.EmbellishEnv { Embellish.phaseId = phaseId
                                        }
    in Embellish.embellishModule embEnv . HsComprehension.Cvt.cvtModule . GhcDump.Convert.cvtModule dflags phaseId phase

projectState :: IORef Capture
projectState = 
    let capture = 
            Capture { captureName = T.empty
                    , captureDate = 0
                    , captureModules = []
                    }
    in unsafePerformIO $ newIORef capture

plugin :: Plugin
plugin = defaultPlugin { installCoreToDos = install }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install options todo = do
    let slug = case options of
                    [slug] -> slug
                    _      -> error "provide a slug for the dump as exactly 1 argument"
    liftIO $ print options
    liftIO $ FP.createDirectoryIfMissing True (coreDumpDir slug)
    modName <- showPprUnsafe <$> getModule
    liftIO $ modifyIORef projectState $ \capture -> 
        capture 
            { captureModules = (T.pack modName, length todo) : captureModules capture 
            , captureName = T.pack slug
            }
    let dumpPasses = zipWith (dumpPass slug) [1..] (map getPhase todo)
    let firstPass = dumpPass slug 0 "Desugared"
    pure $ firstPass : (P.concat $ zipWith (\x y -> [x,y]) todo dumpPasses) ++ [finalPass]


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

dumpPass :: String -> Int -> String -> CoreToDo
dumpPass slug n phase = CoreDoPluginPass "Core Snapshot" $ \in_guts -> do
    guts <- liftIO $ Uniqify.uniqueModule in_guts
    dflags <- getDynFlags
    let prefix :: String = showSDocUnsafe (ppr (mg_module guts))
    let fname = coreDumpFile slug prefix n
    liftIO $ do
        putStrLn fname
        let mod = cvtGhcModule dflags n phase guts
        writeToFile fname mod
    pure guts

finalPass :: CoreToDo
finalPass = CoreDoPluginPass "Finalize dump" $ \guts -> do
    liftIO $ do
        time <- currentPosixMillis
        modifyIORef projectState $ \capture -> capture { captureDate = time }
        readIORef projectState >>= \capture -> 
            writeToFile (captureFile (T.unpack (captureName capture))) capture
    pure guts
