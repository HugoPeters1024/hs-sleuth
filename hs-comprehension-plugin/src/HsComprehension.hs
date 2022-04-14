module HsComprehension (plugin) where

import GHC.Plugins

import qualified GhcDump.Ast as Ast
import GhcDump.Convert (cvtModule)

plugin :: Plugin
plugin = defaultPlugin { installCoreToDos = install }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = pure $ showSugar:todo

showSugar :: CoreToDo
showSugar = CoreDoPluginPass "bruh" $ printCore
    where 
        printCore guts = do
            dflags <- getDynFlags
            liftIO $ print $ cvtModule dflags "Desugar" guts
            pure guts
            





