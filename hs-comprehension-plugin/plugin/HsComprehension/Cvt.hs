{-# LANGUAGE RecordWildCards #-}

module HsComprehension.Cvt where

import HsComprehension.Ast

import Data.Text (Text)
import qualified Data.Text as T

import qualified GhcDump.Ast as GHCD

showText :: Show a => a -> Text
showText = T.pack . show

cvtExternalName :: GHCD.SExternalName -> ExternalName
cvtExternalName GHCD.ExternalName {..} = ExternalName 
    { externalModuleName = GHCD.getModuleName externalModuleName
    , externalType = cvtType externalType
    , .. 
    }

cvtBinder :: GHCD.SBinder -> Binder
cvtBinder sbndr = case GHCD.unSBndr sbndr of
    GHCD.Binder {..} -> Binder
        { binderIdInfo = cvtIdInfo binderIdInfo
        , binderType = cvtType binderType
        , ..
        }
    GHCD.TyBinder {..} -> TyBinder
        { binderKind = cvtType binderKind
        , ..
        }

cvtIdInfo :: GHCD.IdInfo GHCD.SBinder GHCD.BinderId -> IdInfo
cvtIdInfo GHCD.IdInfo {..} = IdInfo
    { idiUnfolding = cvtUnfolding idiUnfolding
    , ..
    }

cvtUnfolding :: GHCD.Unfolding GHCD.SBinder GHCD.BinderId -> Unfolding
cvtUnfolding GHCD.NoUnfolding = NoUnfolding
cvtUnfolding GHCD.BootUnfolding = BootUnfolding
cvtUnfolding (GHCD.OtherCon cons)  = OtherCon (map cvtAltCon cons)
cvtUnfolding GHCD.DFunUnfolding = DFunUnfolding
cvtUnfolding GHCD.CoreUnfolding {..} = CoreUnfolding
    { unfTemplate = cvtExpr unfTemplate
    , ..
    }

cvtExpr :: GHCD.SExpr -> Expr
cvtExpr (GHCD.EVar id) = EVar id
cvtExpr (GHCD.EVarGlobal name) = EVarGlobal (cvtExternalName name)
cvtExpr (GHCD.ELit lit) = ELit (cvtLit lit)
cvtExpr (GHCD.EApp f a) = EApp (cvtExpr f) (cvtExpr a)
cvtExpr (GHCD.ETyLam bndr expr) = ETyLam (cvtBinder bndr) (cvtExpr expr)
cvtExpr (GHCD.ELam bndr expr) = ELam (cvtBinder bndr) (cvtExpr expr)
cvtExpr (GHCD.ELet bs body) = 
    let cvtPair (bndr, expr) = (cvtBinder bndr, cvtExpr expr)
    in ELet (map cvtPair bs) (cvtExpr body)
cvtExpr (GHCD.ECase expr bndr alts) = ECase (cvtExpr expr) (cvtBinder bndr) (map cvtAlt alts)
cvtExpr (GHCD.ETick t expr) = ETick t (cvtExpr expr)
cvtExpr (GHCD.EType t) = EType (cvtType t)
cvtExpr GHCD.ECoercion = ECoercion

cvtLit :: GHCD.Lit -> Lit
cvtLit (GHCD.MachChar c) = MachChar c
cvtLit (GHCD.MachStr t) = MachStr (showText t)
cvtLit (GHCD.MachNullAddr) = MachNullAddr
cvtLit (GHCD.MachInt i) = MachInt (showText i)
cvtLit (GHCD.MachInt64 i) = MachInt64 (showText i)
cvtLit (GHCD.MachWord w) = MachWord (showText w)
cvtLit (GHCD.MachWord64 w) = MachWord64 (showText w)
cvtLit (GHCD.MachFloat f) = MachFloat (showText f)
cvtLit (GHCD.MachDouble d) = MachDouble (showText d)
cvtLit (GHCD.MachLabel l) = MachLabel l
cvtLit (GHCD.LitInteger i) = LitInteger (showText i)
cvtLit (GHCD.LitNatural n) = LitNatural (showText n)
cvtLit (GHCD.LitRubbish) = LitRubbish

cvtAlt :: GHCD.SAlt -> Alt
cvtAlt GHCD.Alt {..} = Alt
    { altCon = cvtAltCon altCon
    , altBinders = map cvtBinder altBinders
    , altRHS = cvtExpr altRHS
    }

cvtAltCon :: GHCD.AltCon -> AltCon
cvtAltCon (GHCD.AltDataCon t) = AltDataCon t
cvtAltCon (GHCD.AltLit lit) = AltLit (cvtLit lit)
cvtAltCon (GHCD.AltDefault) = AltDefault

cvtType :: GHCD.SType -> Type
cvtType (GHCD.VarTy id) = VarTy id
cvtType (GHCD.FunTy f a) = FunTy (cvtType f) (cvtType a)
cvtType (GHCD.TyConApp con ts) = TyConApp con (map cvtType ts)
cvtType (GHCD.AppTy f a) = AppTy (cvtType f) (cvtType a)
cvtType (GHCD.ForAllTy bndr t) = ForAllTy (cvtBinder bndr) (cvtType t)
cvtType (GHCD.LitTy) = LitTy
cvtType (GHCD.CoercionTy) = CoercionTy

cvtTopBinding :: GHCD.STopBinding -> TopBinding
cvtTopBinding tb = 
    let cvtInfo :: (GHCD.SBinder, CoreStats, GHCD.SExpr) -> TopBindingInfo
        cvtInfo (bndr, stats, expr) =
            TopBindingInfo { topBindingBinder = cvtBinder bndr 
                           , topBindingCoreState = stats
                           , topBindingRHS = cvtExpr expr
                           , topBindingFromSource = False
                           }
    in case tb of
         GHCD.NonRecTopBinding bndr stats expr -> NonRecTopBinding (cvtInfo (bndr, stats, expr))
         GHCD.RecTopBinding bs -> RecTopBinding (map cvtInfo bs)

cvtModule :: GHCD.SModule -> Module
cvtModule GHCD.Module {..} = Module 
    { moduleName = GHCD.getModuleName moduleName
    , moduleTopBindings = map cvtTopBinding moduleTopBindings
    , ..
    }
