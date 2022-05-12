{-# LANGUAGE ScopedTypeVariables #-}
module HsComprehension.Plugin where

import Prelude as P
import Data.Maybe
import GHC.Plugins

import HsComprehension.Meta
import HsComprehension.Uniqify as Uniqify

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
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified GhcDump.Ast as Ast
import GhcDump.ToHtml (topBindingsToHtml)
import qualified GhcDump.Convert as Ast (cvtModule)
import GhcDump.Reconstruct (reconModule)

import Data.Time
import Data.Time.Clock.POSIX

millisSinceEpoch :: POSIXTime -> Int
millisSinceEpoch =
    floor . (1e3 *) . nominalDiffTimeToSeconds

currentPosixMillis :: IO Int
currentPosixMillis = millisSinceEpoch <$> getPOSIXTime


projectState :: IORef ProjectMeta
projectState = 
    let projectMeta = ProjectMeta { modules = []
                                  , capturedAt = 0
                                  , slug = T.empty
                                  }
    in unsafePerformIO $ newIORef projectMeta

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
    liftIO $ do
        let mod = ModuleMeta (length todo) (T.pack modName)

        modifyIORef projectState $ \(ProjectMeta ms time _) -> ProjectMeta (mod:ms) time (T.pack slug)
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

projectMetaFile :: String -> FilePath
projectMetaFile pid = coreDumpDir pid ++ "projectmeta.zstd"

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
        let smodule :: Ast.SModule = Ast.cvtModule dflags n phase guts
        writeToFile fname smodule
    pure guts

finalPass :: CoreToDo
finalPass = CoreDoPluginPass "Finalize dump" $ \guts -> do
    liftIO $ do
        time <- currentPosixMillis
        modifyIORef projectState $ \(ProjectMeta mods _ slug) -> ProjectMeta mods time slug
        readIORef projectState >>= \p@(ProjectMeta _ _ slug) -> writeToFile (projectMetaFile (T.unpack slug)) p
    pure guts
