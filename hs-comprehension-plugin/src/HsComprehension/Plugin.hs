{-# LANGUAGE ScopedTypeVariables #-}
module HsComprehension.Plugin (plugin, coreDumpFile) where

import Prelude as P
import Data.Maybe
import GHC.Plugins

import Control.Monad.IO.Class

import qualified Data.ByteString.Lazy as BSL
import qualified Codec.Serialise as Ser

import System.FilePath.Posix as FP
import System.Directory as FP
import System.IO (openFile, IOMode(..))
import Data.ByteString.Lazy (hPutStr)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified GhcDump.Ast as Ast
import GhcDump.ToHtml (topBindingsToHtml)
import qualified GhcDump.Convert as Ast (cvtModule)
import qualified GhcDump.Reconstruct as Ast (reconModule)
import GhcDump.Reconstruct (reconModule)
import HsComprehension.Cvt (cvtModule)

plugin :: Plugin
plugin = defaultPlugin { installCoreToDos = install }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
    liftIO $ FP.createDirectoryIfMissing True coreDumpDir
    let dumpPasses = zipWith dumpPass [1..] (map getPhase todo)
    let firstPass = dumpPass 0 "Desugared"
    pure $ firstPass : (P.concat $ zipWith (\x y -> [x,y]) todo dumpPasses)



getPhase :: CoreToDo -> String
getPhase todo = showSDocUnsafe (ppr todo) ++ " " ++ showSDocUnsafe (pprPassDetails todo)

printPpr :: (Outputable a, MonadIO m) => a -> m ()
printPpr a = liftIO $ putStrLn $ showSDocUnsafe (ppr a)

coreDumpDir :: FilePath
coreDumpDir = "./dist-newstyle/coredump/"

coreDumpFile :: String -> Int -> FilePath
coreDumpFile mod id = coreDumpDir ++ mod ++ "-coredump-pass" ++ show id ++ ".cbor"

dumpPass :: Int -> String -> CoreToDo
dumpPass n phase = CoreDoPluginPass phase $ \guts -> do
    dflags <- getDynFlags
    let prefix :: String = showSDocUnsafe (ppr (mg_module guts))
    let fname = coreDumpFile prefix n
    liftIO $ do
        putStrLn fname
        let smodule :: Ast.SModule = Ast.cvtModule dflags phase guts
        BSL.writeFile (fname) $ Ser.serialise smodule
    pure guts
