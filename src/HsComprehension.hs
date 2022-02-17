{-# LANGUAGE OverloadedStrings #-}
module HsComprehension (plugin) where

import GHC
import GHC.Plugins
import GHC.Core.Ppr

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
               putMsg "||||||||||||||||||||||||||||||||||"
               bindsOnlyPass (mapM (printLet dflags)) guts
               pure guts

    where printLet :: DynFlags -> CoreBind -> CoreM CoreBind
          printLet dflags bndr@(NonRec b e) = do
              putMsgS $ unlines [ "------------------------------"
                                , "Found a nonrec function named " ++ showSDoc dflags (ppr b)
                                , "Pretty: " ++ showBind dflags bndr
                                ]
              pure bndr

          printLet dflags bndr@(Rec _) = do
              putMsg "I can't analyse recursive functions yet..."
              pure bndr

mkSpace :: Int -> String
mkSpace n = concat $ replicate n " "

showBind :: OutputableBndr a => DynFlags -> Bind a -> String
showBind dflags b = unlines $ map (\(n, v) -> mkSpace n ++ v) (showBind' dflags 0 b)


showBind' :: OutputableBndr a => DynFlags -> Int -> Bind a -> [(Int, String)]
showBind' dflags n (NonRec b e) = showExpr' dflags n e
showBind' dflags n (Rec _) = [(n, "recursive functions not supported yet...")]


showExpr :: OutputableBndr a => DynFlags -> Expr a -> String
showExpr dflags e = unlines $ map (\(n, v) -> mkSpace n ++ v) (showExpr' dflags 0 e)

showExpr' :: OutputableBndr a => DynFlags -> Int -> Expr a -> [(Int, String)]
showExpr' dflags = go
    where single = (:[])
          prepend str ((n, line):tl) = (n, str++line):tl
          append  str ((n, line):tl) = (n, line++str):tl
          parens = prepend "(" . append ")"

          go :: OutputableBndr a => Int -> Expr a -> [(Int, String)]
          go n (Var i) = single (n, "Var " ++ showSDocUnsafe (ppr (varName i)))
          go n (Lit lit) = single (n, "Lit " ++ showSDocUnsafe (pprLiteral id lit))
          go n (App e a) = single (n, "App") 
                        ++ parens (go (n+4) e)
                        ++ parens (go (n+4) a)
          go n (Lam b e) = prepend ("λ" ++ showSDoc dflags (ppr b) ++ " -> ") (go (n+4) e)
          go n (Let b@(NonRec b' e') e) = prepend ("let " ++ showSDoc dflags (ppr b') ++ " = ") (showExpr' dflags (n+4) e')
                                       ++ showBind' dflags (n+4) b
                        ++ single (n, "in")
                        ++ go (n+4) e
          go n (Case {}) = single (n, "Kees")
          go n (Cast e _) = go n e
          go n (Tick _ e) = go n e
          go n _          = single (n, "")


parsedPlugin :: [CommandLineOption] -> ModSummary -> HsParsedModule -> Hsc HsParsedModule
parsedPlugin _ _ pm = do
    dflags <- getDynFlags
    liftIO $ putStrLn $ "parsed: \n" ++ showSDoc dflags (ppr (hpm_module pm))
    pure pm
