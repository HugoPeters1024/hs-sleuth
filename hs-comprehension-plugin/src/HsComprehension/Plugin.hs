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


type GState = ProjectMeta

projectState :: IORef GState
projectState = 
    let projectMeta = ProjectMeta { modules = []
                                  }
        uniqEnv = S.empty
    in unsafePerformIO $ newIORef projectMeta

plugin :: Plugin
plugin = defaultPlugin { installCoreToDos = install }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
    liftIO $ FP.createDirectoryIfMissing True coreDumpDir
    modName <- showPprUnsafe <$> getModule
    liftIO $ do
        let mod = ModuleMeta (length todo) (T.pack modName)
        modifyIORef projectState $ \(ProjectMeta ms) -> ProjectMeta (mod:ms)
    let dumpPasses = zipWith dumpPass [1..] (map getPhase todo)
    let firstPass = dumpPass 0 "Desugared"
    pure $ firstPass : (P.concat $ zipWith (\x y -> [x,y]) todo dumpPasses) ++ [finalPass]


getPhase :: CoreToDo -> String
getPhase todo = showSDocUnsafe (ppr todo) ++ " " ++ showSDocUnsafe (pprPassDetails todo)

printPpr :: (Outputable a, MonadIO m) => a -> m ()
printPpr a = liftIO $ putStrLn $ showSDocUnsafe (ppr a)

coreDumpDir :: FilePath
coreDumpDir = "./dist-newstyle/coredump/"

coreDumpFile :: String -> Int -> FilePath
coreDumpFile mod id = coreDumpDir ++ mod ++ "-coredump-pass" ++ show id ++ ".zstd"

projectMetaFile :: FilePath
projectMetaFile = coreDumpDir ++ "projectmeta.zstd"

writeToFile :: (Serialise a) => FilePath -> a -> IO ()
writeToFile fname = do
    BSL.writeFile fname . Zstd.compress 2 . Ser.serialise

readFromFile :: Serialise a => FilePath -> IO a
readFromFile fname = do
    Ser.deserialise . Zstd.decompress <$> BSL.readFile fname

dumpPass :: Int -> String -> CoreToDo
dumpPass n phase = CoreDoPluginPass "Core Snapshot" $ \in_guts -> do
    guts <- liftIO $ Uniqify.uniqueModule in_guts
    dflags <- getDynFlags
    let prefix :: String = showSDocUnsafe (ppr (mg_module guts))
    let fname = coreDumpFile prefix n
    liftIO $ do
        putStrLn fname
        let smodule :: Ast.SModule = Ast.cvtModule dflags n phase guts
        writeToFile fname smodule
    pure guts

finalPass :: CoreToDo
finalPass = CoreDoPluginPass "Finalize dump" $ \guts -> do
    liftIO $ readIORef projectState >>= writeToFile projectMetaFile
    pure guts
