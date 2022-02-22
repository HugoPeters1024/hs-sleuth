{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module HsComprehension (plugin) where

import Control.Monad.State
import Data.Char

import GHC
import GHC.Plugins

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
    pure $ myPass:todo

pass :: ModGuts -> CoreM ModGuts
pass guts = do dflags <- getDynFlags
               bindsOnlyPass (mapM (printLet dflags)) guts
               pure guts

    where printLet :: DynFlags -> CoreBind -> CoreM CoreBind
          printLet dflags bndr@(NonRec b e) = do
              sbndr <- showBind (stripBind bndr)
              putMsgS $ unlines [ "------------------------------"
                                , "Found a nonrec function named " ++ showSDoc dflags (ppr b)
                                , "Pretty: "
                                , sbndr
                                ]
              pure bndr

          printLet dflags bndr@(Rec _) = do
              putMsg "I can't analyse recursive functions yet..."
              pure bndr

intersperse :: a -> [a] -> [a]
intersperse = intersperse' False
    where intersperse' False x ys = x:intersperse' True x ys
          intersperse' True x (y:ys) = y:intersperse' False x ys
          intersperse' True x [] = []

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

parensPP :: OutputableBndr a => Expr a -> CorePP
parensPP e@(Var _) = exprPP e
parensPP e@(Lit _) = exprPP e
parensPP e = printPP "(" >> exprPP e >> printPP ")"

printPP :: String -> CorePP
printPP s = modify $ \CorePPState {..} -> CorePPState { output = output ++ s, ..}

showPP :: SDoc -> CorePP
showPP sdoc = do
    dflags <- gets dynFlags
    printPP $ showSDoc dflags sdoc

evalPP :: DynFlags -> CorePP -> CoreM String
evalPP dflags pp = output <$> execStateT pp (CorePPState dflags 0 mempty)

showBind :: OutputableBndr a => Bind a -> CoreM String
showBind b = getDynFlags >>= \dflags -> showBind' dflags b

showBind' :: OutputableBndr a => DynFlags -> Bind a -> CoreM String
showBind' dflags b = evalPP dflags $ bindPP b

showExpr :: OutputableBndr a => DynFlags -> Expr a -> CoreM String
showExpr dflags e = evalPP dflags $ exprPP e


bindPP :: OutputableBndr a => Bind a -> CorePP
bindPP (NonRec b e) = exprPP e
bindPP (Rec _) = printPP "recursive functions not supported yet..."

exprPP :: OutputableBndr a => Expr a -> CorePP
exprPP (Var i) = showPP (ppr i)
exprPP (Lit lit) = showPP (ppr lit)
exprPP (App e@(Var ev) a) = let opName = showSDocUnsafe (ppr (varName ev))
                             in if any isLetter opName 
                                    then exprPP e >> printPP " " >> parensPP a
                                    else parensPP a >> printPP " " >> exprPP e >> printPP " "
exprPP (App e a) = exprPP e >> printPP " " >> parensPP a
exprPP (Lam b e) = do
    printPP "λ"
    showPP (ppr b)
    printPP " -> "
    indented $ exprPP e
exprPP (Let b@(NonRec b' e') e) = do
    printPP "let "
    showPP (ppr b')
    printPP " = "
    exprPP e'
    indented $ do
        printPP " in "
        exprPP e
exprPP (Let b@(Rec _) e) = printPP "RECURSIVE LET NOT IMPLEMENTED"
exprPP (Case e _ _ alts) = do
    printPP "Case " 
    exprPP e 
    printPP " of" 
    indented $ mapM_ (\alt -> altPP alt >> newline) alts
exprPP (Cast e _) = exprPP e
exprPP (Tick _ e) = printPP "Tick"
exprPP (Type t) = printPP "Type " >> showPP (ppr t)
exprPP (Coercion _) = printPP "Coercion"

altPP :: OutputableBndr a => Alt a -> CorePP
altPP (Alt con _ expr) = showPP (ppr con) >> printPP " -> " >> exprPP expr


parsedPlugin :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
parsedPlugin _ ms pm = do
    dflags <- getDynFlags
    liftIO $ putStrLn $ "parsed: \n" ++ showSDoc dflags (ppr (hpm_module pm))
    pure pm

stripBind :: Bind a -> Bind a
stripBind (NonRec b e) = NonRec b (stripExpr e)
stripBind _ = error "TODO: recursive binds"

stripExpr :: Expr a -> Expr a
stripExpr (Var i) = Var i
stripExpr (Lit l) = Lit l
stripExpr (App e (Type _)) = stripExpr e
stripExpr (App e (Var i)) = case showSDocUnsafe (ppr (varName i)) of
    ('$':_) -> stripExpr e
    _       -> App (stripExpr e) (Var i)
stripExpr (App e a) = App (stripExpr e) (stripExpr a)

stripExpr (Lam b e) = Lam b (stripExpr e)
stripExpr (Let b e) = Let (stripBind b) (stripExpr e)
stripExpr x = x


