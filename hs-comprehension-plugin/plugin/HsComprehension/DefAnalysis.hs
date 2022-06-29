module HsComprehension.DefAnalysis (defAnalysis) where

import HsComprehension.Ast

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as M

data TState = TState
    { currentPhase :: Int
    , binderEnv :: Map BinderId Int
    }

type DefA a = State TState a

topBindingGetBinders :: TopBinding -> [Binder]
topBindingGetBinders (NonRecTopBinding x) = [topBindingBinder x]
topBindingGetBinders (RecTopBinding xs)   = map topBindingBinder xs

nextPhase :: DefA ()
nextPhase = modify $ \s -> s { currentPhase = 1 + (currentPhase s) }


lookupBinderPhase :: BinderId -> DefA Int
lookupBinderPhase bid = do
    s <- get
    case M.lookup bid (binderEnv s) of
      Nothing -> pure (currentPhase s)
      Just x -> pure x

addBinding :: Binder -> DefA ()
addBinding b = do
    s <- get
    when (M.notMember (binderId b) (binderEnv s)) $ do
        modify $ \s -> s { binderEnv = M.insert (binderId b) (currentPhase s) (binderEnv s)}

defAnalysis :: [Phase] -> [Phase]
defAnalysis xs = let env = TState 0 M.empty
     in (flip evalState env) $ forM xs $ \phase -> do
         phase' <- defPhase phase
         nextPhase
         pure phase'

defPhase :: Phase -> DefA Phase
defPhase phase = do
    -- record each top binding binder at the same time
    mapM_ addBinding (concatMap topBindingGetBinders (phaseTopBindings phase))
    (\tb -> phase { phaseTopBindings = tb}) <$> mapM defTopBinding (phaseTopBindings phase)

defTopBinding :: TopBinding -> DefA TopBinding
defTopBinding (NonRecTopBinding x) = NonRecTopBinding <$> defTopBindingInfo x
defTopBinding (RecTopBinding xs) = RecTopBinding <$> mapM defTopBindingInfo xs

defTopBindingInfo :: TopBindingInfo -> DefA TopBindingInfo
defTopBindingInfo topInfo = (\b e -> topInfo { topBindingBinder = b, topBindingRHS = e}) 
    <$> defBinding (topBindingBinder topInfo)
    <*> defExpr (topBindingRHS topInfo)

defBinding :: Binder -> DefA Binder
defBinding b@(Binder {}) = (\i -> b { binderCreatedPhaseId = i }) <$> lookupBinderPhase (binderId b)
defBinding b = pure b

defExpr :: Expr -> DefA Expr
defExpr (EVar var) = pure (EVar var)
defExpr (EVarGlobal gname) = pure (EVarGlobal gname)
defExpr (ELit lit) = pure (ELit lit)
defExpr (EApp f a) = EApp <$> defExpr f <*> defExpr a
defExpr (ETyLam b e) = ETyLam b <$> defExpr e
defExpr (ELam b e) = addBinding b >> ELam <$> defBinding b <*> defExpr e
defExpr (ELet bses rhs) = do
    let (bs, es) = unzip bses
    mapM_ addBinding bs
    ELet <$> (zip <$> mapM defBinding bs <*> mapM defExpr es) <*> defExpr rhs
defExpr (ECase e b alts) = addBinding b >> ECase <$> defExpr e <*> defBinding b <*> mapM defAlt alts
defExpr (ETick t e) = ETick t <$> defExpr e
defExpr (EType t) = pure (EType t)
defExpr (ECoercion) = pure ECoercion
defExpr (EMarkDiff e) = EMarkDiff <$> defExpr e

defAlt :: Alt -> DefA Alt
defAlt alt = do
    mapM_ addBinding (altBinders alt)
    (\bs rhs -> alt { altBinders = bs, altRHS = rhs}) <$> mapM defBinding (altBinders alt) <*> defExpr (altRHS alt) 
