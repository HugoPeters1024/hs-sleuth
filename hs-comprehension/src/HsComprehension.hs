{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module HsComprehension (plugin, CoreTrace(..)) where

import Prelude hiding ((<>))
import Control.Monad.State
import Data.Char
import Data.List
import Data.Maybe
import Data.IORef

import GHC
import GHC.Core.Map.Expr
import GHC.Types.Var
import GHC.Types.Tickish
import GHC.Plugins
import Data.Data

import Generation

data CoreTrace = CoreTrace deriving (Show, Data)

data CorePPState = CorePPState { dynFlags :: DynFlags
                               , indent :: Int
                               , output :: String
                               }

type CorePP = StateT CorePPState CoreM ()

type PluginState = CoreMap String

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
    state <- liftIO $ newIORef emptyCoreMap
    let myPass = \prevName -> CoreDoPluginPass "Nothing much" (pass state prevName)
    let firstPass = myPass "Desugared" 
    let passes = concat $ zipWith (\x y -> [x, y (ppr x)]) todo (repeat myPass)
    pure (firstPass : passes ++ [printInfoPass])

getAllTopLevelDefs :: [CoreBind] -> [(CoreBndr, CoreExpr)]
getAllTopLevelDefs binds = concatMap go binds
    where go (NonRec b e) = [(b,e)]
          go (Rec bs) = bs


annotationsOn :: forall a. Data a => ModGuts -> CoreBndr -> CoreM [a]
annotationsOn guts bndr = do
  (_, anns) <- getAnnotations (deserializeWithData @a) guts
  return $ lookupWithDefaultUFM_Directly anns [] (varUnique bndr)

isCoreTraced :: ModGuts -> CoreBndr -> CoreM Bool
isCoreTraced guts b = (not . null) <$> annotationsOn @CoreTrace guts b

annotatedFunctions :: ModGuts -> [(CoreBndr, CoreExpr)] -> CoreM [String]
annotatedFunctions guts = mapMaybeM f
    where f :: (CoreBndr, CoreExpr) -> CoreM (Maybe String)
          f (b, _) = do
              dflags <- getDynFlags
              anns <- annotationsOn @CoreTrace guts b
              pure $ if length anns > 0 
                        then Just (showSDoc dflags (ppr b))
                        else Nothing

printInfoPass :: CoreToDo
printInfoPass = CoreDoPluginPass "Print Info" $ \guts -> do
    liftIO $ saveToFile pageTest
    pure guts


pass :: IORef PluginState -> SDoc -> ModGuts -> CoreM ModGuts
pass state prevName guts = do dflags <- getDynFlags
                              let binders = getAllTopLevelDefs (mg_binds guts)
                              annFs <- annotatedFunctions guts binders
                              putMsgS "--------------------------"
                              putMsg prevName
                              putMsgS "--------------------------"
                              putMsgS $ "attempting to locate: " ++ show annFs
                              putMsg $ cat $ map (ppr . fst) binders
                              mapM (go annFs) binders
                              pure guts

    where go :: [String] -> (CoreBndr, CoreExpr) -> CoreM ()
          go toTrace (b, e) = do
              dflags <- getDynFlags
              let fname = showSDoc dflags (ppr b)
              known <- isJust . (flip lookupCoreMap e) <$> liftIO (readIORef state)
              liftIO $ modifyIORef state (\m -> extendCoreMap m e fname)
              --when (not known) $ do
              when (or (map (\tc -> isSubsequenceOf tc fname) toTrace)) $ do
                  sexpr <- showExpr dflags (stripExpr e)
                  putMsgS $ unlines [ "Found annotated function named " ++ showSDoc dflags (ppr b)
                                    , "Pretty: "
                                    , sexpr
                                    ]
              


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

evalPP :: DynFlags -> CorePP -> CoreM String
evalPP dflags pp = output <$> execStateT pp (CorePPState dflags 0 mempty)

showBind :: CoreBind -> CoreM String
showBind b = getDynFlags >>= \dflags -> evalPP dflags (bindPP b)

showExpr :: DynFlags -> CoreExpr -> CoreM String
showExpr dflags e = evalPP dflags $ exprPP e


bindPP :: CoreBind -> CorePP
bindPP (NonRec b e) = showPP (ppr b) >> printPP " = " >> exprPP e
bindPP (Rec [(b,e)]) = showPP (ppr b) >> printPP " = " >> exprPP e
bindPP (Rec _) = printPP "co-recursive functions not supported yet..."

exprPP :: CoreExpr -> CorePP
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
exprPP (Type t) = pure () --printPP "@" >> showPP (ppr t)
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


