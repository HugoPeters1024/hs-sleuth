{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module HsComprehension (plugin) where

import Control.Monad.State

import GHC
import GHC.Plugins
import GHC.Core.Ppr

data CorePPState = CorePPState { dynFlags :: DynFlags
                               , indent :: Int
                               , output :: String
                               }

type CorePP = StateT CorePPState CoreM ()

plugin :: Plugin
plugin = defaultPlugin
  { installCoreToDos = install
  , parsedResultAction = parsedPlugin
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
    let myPass = CoreDoPluginPass "Nothing much" pass
    pure (myPass:todo)

pass :: ModGuts -> CoreM ModGuts
pass guts = do dflags <- getDynFlags
               bindsOnlyPass (mapM (printLet dflags)) guts
               pure guts

    where printLet :: DynFlags -> CoreBind -> CoreM CoreBind
          printLet dflags bndr@(NonRec b e) = do
              sbndr <- showBind dflags bndr
              putMsgS $ unlines [ "------------------------------"
                                , "Found a nonrec function named " ++ showSDocDump dflags (ppr b)
                                , "Pretty: " ++ sbndr
                                ]
              pure bndr

          printLet dflags bndr@(Rec _) = do
              putMsg "I can't analyse recursive functions yet..."
              pure bndr

mkSpace :: Int -> String
mkSpace n = concat $ replicate n " "

newline :: CorePP
newline = modify $ \CorePPState {..} -> CorePPState { output = output ++ "\n" ++ mkSpace indent, ..}

indented :: CorePP -> CorePP
indented pp = do
    modify $ \CorePPState {..} -> CorePPState { indent = indent + 4, ..}
    newline
    pp
    modify $ \CorePPState {..} -> CorePPState { indent = indent - 4, ..}
    newline

parensPP :: CorePP -> CorePP
parensPP pp = printPP "(" >> pp >> printPP ")"

printPP :: String -> CorePP
printPP s = modify $ \CorePPState {..} -> CorePPState { output = output ++ s, ..}

showPP :: SDoc -> CorePP
showPP sdoc = do
    dflags <- gets dynFlags
    printPP $ showSDoc dflags sdoc

evalPP :: DynFlags -> CorePP -> CoreM String
evalPP dflags pp = output <$> execStateT pp (CorePPState dflags 0 mempty)

showBind :: OutputableBndr a => DynFlags -> Bind a -> CoreM String
showBind dflags b = evalPP dflags $ bindPP b

showExpr :: OutputableBndr a => DynFlags -> Expr a -> CoreM String
showExpr dflags e = evalPP dflags $ exprPP e


bindPP :: OutputableBndr a => Bind a -> CorePP
bindPP (NonRec b e) = exprPP e
bindPP (Rec _) = showPP "recursive functions not supported yet..."


exprPP :: OutputableBndr a => Expr a -> CorePP
exprPP (Var i) = showPP (ppr i)
exprPP (Lit lit) = showPP (ppr lit)
exprPP (App e a) = do
    showPP "App"
    indented $ do
        parensPP $ showPP (ppr e)
        newline
        parensPP $ showPP (ppr e)
exprPP (Lam b e) = do
    printPP "λ"
    showPP (ppr b)
    printPP " -> "
    indented $ showPP (ppr e)
exprPP (Let b@(NonRec b' e') e) = do
    printPP "let "
    showPP (ppr b')
    printPP " = "
    showPP (ppr e')
    printPP " in "
    showPP (ppr e)
exprPP (Let b@(Rec _) e) = printPP "RECURSIVE LET NOT IMPLEMENTED"
exprPP (Case {}) = printPP "Kees"
exprPP (Tick _ e) = printPP "Tick"



parsedPlugin :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
parsedPlugin _ _ pm = do
    dflags <- getDynFlags
    liftIO $ putStrLn $ "parsed: \n" ++ showSDoc dflags (ppr (hpm_module pm))
    pure pm
