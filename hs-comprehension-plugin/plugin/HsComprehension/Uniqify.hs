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
#if MIN_VERSION_ghc(9,0,0)
import GHC.Plugins
#else
import GhcPlugins
#endif

#if MIN_VERSION_ghc(9,0,0)
import GHC.Types.Unique (mkUnique, unpkUnique)
#else
import Unique (mkUnique, unpkUnique)
#endif

-- A local scope map and a global unique set
data UniqEnv = UniqEnv
  { uenv_scope :: Map Int Int
  , uenv_global :: Set Int
  }

emptyEnv = UniqEnv
  { uenv_scope = M.empty
  , uenv_global = S.empty
  }

uenvInsertScope :: Int -> Int -> UniqEnv -> UniqEnv
uenvInsertScope from to env = env { uenv_scope = M.insert from to (uenv_scope env) }

uenvInsertGlobal :: Int -> UniqEnv -> UniqEnv
uenvInsertGlobal uid env = env { uenv_global = S.insert uid (uenv_global env) }


type Uniq a = StateT UniqEnv IO a

runUnique :: Uniq a -> IO a
runUnique uq = evalStateT uq emptyEnv

uniqueModule :: ModGuts -> IO ModGuts
uniqueModule guts@ModGuts { mg_binds = mg_binds } = do
    nbinds <- runUnique (uniqProgram mg_binds)
    pure $ guts { mg_binds = nbinds }

-- Prevent scoped substitutions to escape 
limitScope :: Uniq a -> Uniq a
limitScope g = do
    scope_before <- gets uenv_scope
    r <- g
    env_after <- get
    put $ env_after { uenv_scope = scope_before }
    pure r

uniqVar :: Var -> Uniq Var
uniqVar var = 
    if isTyVar var
    then pure var
    else do
      scope <- gets uenv_scope
      let (tag, uid) = unpkUnique (getUnique var)
      -- Lookup if the unique must change
      case M.lookup uid scope of
        Just i -> pure $ setVarUnique var (mkUnique tag i)
        Nothing -> pure var

uniqBndr :: CoreBndr -> Uniq Var
uniqBndr var = do
    if isTyVar var
       then pure var
       else do
        env <- get
        let (tag, uid) = unpkUnique (getUnique var)
        if S.member uid (uenv_global env)
           then do
              -- We've seen a different binding site with this unique before, generate a new one
              new_id <- (`mod` 100000000) <$> randomIO @Int
              modify $ uenvInsertScope uid new_id
              modify $ uenvInsertGlobal new_id
              uniqVar var
           else do
              modify $ uenvInsertGlobal uid
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
uniqExpr (App f a) = App <$> uniqExpr f <*> uniqExpr a
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
