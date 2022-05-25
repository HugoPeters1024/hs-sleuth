module HsComprehension.Embellish where

import HsComprehension.Ast

import Control.Monad
import Control.Monad.Reader
import qualified Data.Text as T

data EmbellishEnv = EmbellishEnv
    { phaseId :: Int
    }

type Emb = Reader EmbellishEnv

eExternalName :: ExternalName -> Emb ExternalName
eExternalName = pure

eBinder :: Binder -> Emb Binder
eBinder = pure

eIdInfo :: IdInfo -> Emb IdInfo
eIdInfo = pure

eUnfolding :: Unfolding -> Emb Unfolding
eUnfolding = pure

eExpr :: Expr -> Emb Expr
eExpr = pure

eLit :: Lit -> Emb Lit
eLit = pure 

eAlt :: Alt -> Emb Alt
eAlt = pure

eType :: Type -> Emb Type
eType = pure

eTopBinding :: TopBinding -> Emb TopBinding
eTopBinding (NonRecTopBinding i) = NonRecTopBinding <$> eTopBidingInfo i
eTopBinding (RecTopBinding is) = RecTopBinding <$> mapM eTopBidingInfo is

eTopBidingInfo :: TopBindingInfo -> Emb TopBindingInfo
eTopBidingInfo info = do
    fromSource <- asks phaseId >>= \phaseId -> if phaseId == 0
                  then case topBindingBinder info of
                         Binder { binderName = name } ->
                           case T.unpack name of
                             '$':_ -> pure False
                             _   -> pure True
                         _ -> pure False
                  else pure $ topBindingFromSource info
                        

    pure $ info { topBindingFromSource = fromSource }


eModule :: Module -> Emb Module
eModule mod@(Module { moduleTopBindings = ts }) = do
    ts' <- mapM eTopBinding ts
    pure $ mod { moduleTopBindings = ts'}


embellishModule :: EmbellishEnv -> Module -> Module
embellishModule env mod = runReader (eModule mod) env
