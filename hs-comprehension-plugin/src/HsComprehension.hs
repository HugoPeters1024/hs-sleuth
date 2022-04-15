module HsComprehension (plugin) where

import GHC.Plugins

import System.IO (openFile, IOMode(..))
import Data.ByteString.Lazy (hPutStr)
import Data.Aeson.Encode.Pretty (encodePretty)
import qualified GhcDump.Ast as Ast
import GhcDump.ToHtml (topBindingsToHtml)
import qualified GhcDump.Convert as Ast (cvtModule)
import GhcDump.Reconstruct (reconModule)
import HsComprehension.Cvt (cvtModule)

plugin :: Plugin
plugin = defaultPlugin { installCoreToDos = install }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = pure $ showSugar:todo

showSugar :: CoreToDo
showSugar = CoreDoPluginPass "bruh" $ printCore
    where 
        printCore guts = do
            dflags <- getDynFlags
            handle <- liftIO $ openFile "output.json" WriteMode
            let json = encodePretty $ cvtModule $ reconModule $ Ast.cvtModule dflags "Desugar" guts
            liftIO $ hPutStr handle json
            pure guts
            





