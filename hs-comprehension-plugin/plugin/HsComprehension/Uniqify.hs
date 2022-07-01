{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}
module HsComprehension.Uniqify
    ( uniqueModule
    ) where

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import System.Random
import Control.Monad.State

import GHC
import GHC.Plugins
import GHC.Types.Unique

-- A local scope map and a global unique set
type UniqEnv = (Map Int Int, Set Int)
type Uniq a = StateT UniqEnv IO a

uniqueModule :: ModGuts -> IO ModGuts
uniqueModule guts@ModGuts { mg_binds = mg_binds } = do
    (nbinds, (scope, nset)) <- runStateT (mapM uniqBind mg_binds) (M.empty, S.empty)
    pure $ guts { mg_binds = nbinds }

runUnique :: Uniq a -> IO a
runUnique uq = evalStateT uq (M.empty, S.empty)

-- Prevent scoped substitutions to escape 
limitScope :: Uniq a -> Uniq a
limitScope g = do
    (s, _) <- get
    r <- g
    (_, gl) <- get
    put (s, gl)
    pure r

uniqVar :: Var -> Uniq Var
uniqVar var = do
    (scope, _) <- get
    let (tag, uid) = unpkUnique (getUnique var)
    case M.lookup uid scope of
      Just i -> pure $ setVarUnique var (mkUnique tag i)
      Nothing -> pure var

uniqBndr :: CoreBndr -> Uniq Var
uniqBndr var = do
    if isTyVar var
       then pure var
       else do
        (scope, gl) <- get
        let (tag, uid) = unpkUnique (getUnique var)
        if S.member uid gl
           then do
              idx <- (`mod` 10000000) <$> randomIO @Int
              let nscope = M.insert uid idx scope
              let ngl = S.insert idx gl
              put (nscope, ngl)
              pure $ setVarUnique var (mkUnique tag idx)
           else do
               let gl' = S.insert uid gl
               put (scope, gl')
               pure var

uniqProgram :: CoreProgram -> Uniq CoreProgram
uniqProgram = mapM uniqBind

uniqBind :: CoreBind -> Uniq CoreBind
uniqBind (NonRec b e) = NonRec <$> uniqBndr b <*> uniqExpr e
uniqBind (Rec xs) = do
    let (bs, es) = unzip xs
    bs' <- mapM uniqBndr bs
    es' <- mapM uniqExpr es
    pure $ Rec $ zip bs' es'

uniqExpr :: CoreExpr -> Uniq CoreExpr
uniqExpr (Var var) = Var <$> uniqVar var
uniqExpr (Lit lit) = pure $ Lit lit
uniqExpr (App e a) = App <$> uniqExpr e <*> uniqExpr a
uniqExpr (Lam b e) = limitScope $ Lam <$> uniqBndr b <*> uniqExpr e
uniqExpr (Let b e) = limitScope $ Let <$> uniqBind b <*> uniqExpr e
uniqExpr (Case e b t alts) = limitScope $ Case <$> uniqExpr e <*> uniqBndr b <*> pure t <*> mapM uniqAlt alts
uniqExpr (Cast e c) = Cast <$> uniqExpr e <*> pure c
uniqExpr (Tick t e) = Tick t <$> uniqExpr e
uniqExpr (Type t) = pure $ Type t
uniqExpr (Coercion c) = pure $ Coercion c

uniqAlt :: CoreAlt -> Uniq CoreAlt
#if MIN_VERSION_ghc(9,2,0)
uniqAlt (Alt con bs e) = limitScope $ Alt con <$> mapM uniqBndr bs <*> uniqExpr e
#else
uniqAlt (con, bs, e) = limitScope $ (,,) con <$> mapM uniqBndr bs <*> uniqExpr e
#endif
