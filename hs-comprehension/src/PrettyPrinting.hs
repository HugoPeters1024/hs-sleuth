{-# LANGUAGE RecordWildCards #-}
module PrettyPrinting (
    showExpr,
    showBind
    ) where

import Data.Char
import Control.Monad.State

import GHC
import GHC.Plugins



data CorePPState = CorePPState { dynFlags :: DynFlags
                               , indent :: Int
                               , output :: String
                               }

type CorePP = State CorePPState ()

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

parensPP :: CoreExpr -> CorePP
parensPP e@(Var _) = exprPP e
parensPP e@(Lit _) = exprPP e
parensPP e = printPP "(" >> exprPP e >> printPP ")"

printPP :: String -> CorePP
printPP s = modify $ \CorePPState {..} -> CorePPState { output = output ++ s, ..}

showPP :: SDoc -> CorePP
showPP sdoc = do
    dflags <- gets dynFlags
    printPP $ showSDoc dflags sdoc

evalPP :: DynFlags -> CorePP -> String
evalPP dflags pp = output $ execState pp (CorePPState dflags 0 mempty)

showBind :: DynFlags -> CoreBind -> String
showBind dflags b = evalPP dflags (bindPP b)

showExpr :: DynFlags -> CoreExpr -> String
showExpr dflags e = evalPP dflags (exprPP e)


bindPP :: CoreBind -> CorePP
bindPP (NonRec b e) = showPP (ppr b) >> printPP " = " >> exprPP e
bindPP (Rec []) = pure ()
bindPP (Rec ((b,e):tl)) = showPP (ppr b) >> printPP " = " >> exprPP e >> bindPP (Rec tl)

exprPP :: CoreExpr -> CorePP
exprPP (Var i) = showPP (ppr i)
exprPP (Lit lit) = showPP (ppr lit)
exprPP (App e a) = let
    infixSymbols = "!$%&*+./<=>?@\\^-~"
    isInfixOperator = case e of
                        Var i -> any (\x -> elem x infixSymbols) $ showSDocUnsafe $ ppr $ varName i
                        _ -> False


    isTypeApplication = case a of
                          Type _ -> True
                          _ -> False

    in if isTypeApplication
          then exprPP e
          else if isInfixOperator 
                  then parensPP a >> printPP " " >> exprPP e
                  else exprPP e >> printPP " " >> parensPP a
exprPP (Lam b e) = do
    printPP "\\"
    showPP (ppr b)
    printPP " -> "
    indented $ exprPP e
exprPP (Let b e) = do
    printPP "let "
    bindPP b
    printPP " in"
    indented $ do
        exprPP e
exprPP (Case e _ _ alts) = do
    printPP "case " 
    exprPP e 
    printPP " of" 
    indented $ mapM_ (\alt -> altPP alt >> newline) (reverse alts)
exprPP (Cast e _) = exprPP e
exprPP (Tick _ e) = exprPP e
exprPP (Type t) = printPP "@" >> showPP (ppr t)
exprPP (Coercion _) = printPP "Coercion"

altPP :: CoreAlt -> CorePP
altPP (Alt con bs expr) = do
    if con == DEFAULT
       then printPP "_"
       else showPP (ppr con) 
    printPP " " 
    mapM (\b -> (showPP (ppr b)) >> printPP " ") bs 
    printPP "-> " 
    exprPP expr


stripBind :: Bind a -> Bind a
stripBind (NonRec b e) = NonRec b (stripExpr e)
stripBind b = b

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
