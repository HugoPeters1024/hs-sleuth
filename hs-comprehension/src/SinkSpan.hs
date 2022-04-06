{-# LANGUAGE OverloadedStrings #-}
module SinkSpan (sinkSpan) where

import Data.Semigroup ((<>))
import Prelude hiding ((<>))
import GHC
import GHC.Plugins hiding ((<>))

instance Semigroup RealSrcSpan where
    lspan <> rspan =
        let sameFile = srcSpanFile lspan == srcSpanFile rspan
            name = srcSpanFile lspan
            loc1 = mkRealSrcLoc 
                        name 
                        (min (srcSpanStartLine lspan) (srcSpanStartLine rspan))
                        (min (srcSpanStartCol lspan) (srcSpanEndCol rspan))
            loc2 = mkRealSrcLoc 
                        name 
                        (max (srcSpanEndLine lspan) (srcSpanEndLine rspan))
                        (max (srcSpanEndCol lspan) (srcSpanEndCol rspan))
        in if sameFile
           then mkRealSrcSpan loc1 loc2
           else error "spans of different files do not flock together"

instance Semigroup SrcSpan where
    UnhelpfulSpan _ <> rhs = rhs
    lhs <> UnhelpfulSpan _ = lhs
    RealSrcSpan lspan lbuf <> RealSrcSpan rspan rbuf = RealSrcSpan (lspan <> rspan) (lbuf <> rbuf)

setIdLoc :: Id -> SrcSpan -> Id
setIdLoc var span = 
    let name = setNameLoc (getName var) span
    in setIdName var name

sinkSpan :: CoreBind -> CoreBind
sinkSpan bind@(NonRec b e) = 
    let span = sinkSpanBind bind 
    in NonRec (setIdLoc b span) e

sinkSpanId :: Id -> SrcSpan
sinkSpanId var = getSrcSpan var

sinkSpanBndr :: Id -> SrcSpan
sinkSpanBndr = sinkSpanId

sinkSpanBind :: CoreBind -> SrcSpan 
sinkSpanBind (NonRec b e) = sinkSpanBndr b <> sinkSpanExpr e

collapseSemigroup :: Semigroup a => [a] -> a -> a
collapseSemigroup xs init = foldr (<>) init xs

x |> y = y x

sinkSpanExpr :: CoreExpr -> SrcSpan
sinkSpanExpr (Var var) = sinkSpanId var
sinkSpanExpr (Lit l) = noSrcSpan
sinkSpanExpr (App e a) = sinkSpanExpr e <> sinkSpanExpr a
sinkSpanExpr (Lam b e) = sinkSpanBndr b <> sinkSpanExpr e
sinkSpanExpr (Let b e) = sinkSpanBind b <> sinkSpanExpr e
sinkSpanExpr (Case e b t alts) = sinkSpanExpr e <> sinkSpanBndr b |> collapseSemigroup (map sinkSpanAlt alts)
sinkSpanExpr (Cast e _) = sinkSpanExpr e 
sinkSpanExpr (Tick _ e) = sinkSpanExpr e
sinkSpanExpr (Type _) = noSrcSpan
sinkSpanExpr (Coercion c) = noSrcSpan

sinkSpanAlt :: CoreAlt -> SrcSpan
sinkSpanAlt (Alt con bs e) = sinkSpanExpr e |> collapseSemigroup (map sinkSpanBndr bs)



