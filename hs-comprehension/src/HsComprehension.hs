{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module HsComprehension (plugin, CoreTrace(..)) where

import Control.Monad.State
import Data.Char
import Data.List
import Data.Maybe

import GHC
import GHC.Plugins
import Data.Data

data CoreTrace = CoreTrace deriving (Show, Data)

data CorePPState = CorePPState { dynFlags :: DynFlags
                               , indent :: Int
                               , output :: String
                               }

type CorePP = StateT CorePPState CoreM ()

mapMaybeM :: Monad m => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f [] =  pure []
mapMaybeM f (x:xs) = f x >>= \case
    Just x -> (x:) <$> mapMaybeM f xs
    Nothing -> mapMaybeM f xs


plugin :: Plugin
plugin = defaultPlugin
  { installCoreToDos = install
  , parsedResultAction = parsedPlugin
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
    let myPass = \prevName -> CoreDoPluginPass "Nothing much" (pass prevName)
    let firstPass = myPass "Desugared" 
    let passes = concat $ zipWith (\x y -> [x, y (ppr x)]) todo (repeat myPass)
    pure (firstPass : passes)

annotationsOn :: forall a. Data a => ModGuts -> CoreBndr -> CoreM [a]
annotationsOn guts bndr = do
  (_, anns) <- getAnnotations (deserializeWithData @a) guts
  return $ lookupWithDefaultUFM_Directly anns [] (varUnique bndr)

isCoreTraced :: ModGuts -> CoreBind -> CoreM Bool
isCoreTraced guts (NonRec b _) = annotationsOn @CoreTrace guts b >>= \anns -> pure $ (length anns) > 0
isCoreTraced guts (Rec [(b,_)]) = annotationsOn @CoreTrace guts b >>= \anns -> pure $ (length anns) > 0

annotatedFunctions :: ModGuts -> CoreM [String]
annotatedFunctions guts = mapMaybeM f (mg_binds guts)
    where f :: CoreBind -> CoreM (Maybe String)
          f (NonRec b _ ) = do
              dflags <- getDynFlags
              anns <- annotationsOn @CoreTrace guts b
              pure $ if length anns > 0 
                        then Just (showSDoc dflags (ppr b))
                        else Nothing



pass :: SDoc -> ModGuts -> CoreM ModGuts
pass prevName guts = do dflags <- getDynFlags
                        annFs <- annotatedFunctions guts
                        putMsgS "--------------------------"
                        putMsg prevName
                        putMsgS "--------------------------"
                        bindsOnlyPass (mapM (printLet annFs)) guts
                        pure guts

    where printLet :: [String] -> CoreBind -> CoreM CoreBind
          printLet toTrace bndr@(NonRec b e) = do
              dflags <- getDynFlags
              let fname = showSDoc dflags (ppr b)
              when (and (map (\tc -> isPrefixOf tc fname) toTrace)) $ do
                  sbndr <- showBind (stripBind bndr)
                  putMsgS $ unlines [ "Found annotated nonrec function named " ++ showSDoc dflags (ppr b)
                                    , "Pretty: "
                                    , sbndr
                                    ]
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
bindPP (Rec [(b,e)]) = exprPP e
bindPP (Rec _) = printPP "co-recursive functions not supported yet..."

exprPP :: OutputableBndr a => Expr a -> CorePP
exprPP (Var i) = showPP (ppr i)
exprPP (Lit lit) = showPP (ppr lit)
exprPP (App e@(Var ev) a) = let opName = showSDocUnsafe (ppr (varName ev))
                             in if any isLetter opName 
                                    then exprPP e >> printPP " " >> parensPP a
                                    else parensPP a >> printPP " " >> exprPP e
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
    printPP " in"
    indented $ do
        exprPP e
exprPP (Let b@(Rec _) e) = printPP "RECURSIVE LET NOT IMPLEMENTED"
exprPP (Case e _ _ alts) = do
    printPP "case " 
    exprPP e 
    printPP " of" 
    indented $ mapM_ (\alt -> altPP alt >> newline) alts
exprPP (Cast e _) = exprPP e
exprPP (Tick _ e) = printPP "Tick"
exprPP (Type t) = pure () --printPP "@" >> showPP (ppr t)
exprPP (Coercion _) = printPP "Coercion"

altPP :: OutputableBndr a => Alt a -> CorePP
altPP (Alt con bs expr) = do
    showPP (ppr con) 
    printPP " " 
    mapM (\b -> (showPP (ppr b)) >> printPP " ") bs 
    printPP "-> " 
    exprPP expr


parsedPlugin :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
parsedPlugin _ ms pm = do
    dflags <- getDynFlags
    liftIO $ putStrLn $ "parsed: \n" ++ showSDoc dflags (ppr (hpm_module pm))
    pure pm

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


