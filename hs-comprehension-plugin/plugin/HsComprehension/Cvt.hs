{-# LANGUAGE RecordWildCards #-}

module HsComprehension.Cvt where

import HsComprehension.Ast

import Data.Text (Text)
import qualified Data.Text as T

import Data.Hashable as Hash

import qualified GhcDump.Ast as GHCD

data CvtEnv = CvtEnv 
    { cvtEnvPhaseId :: Int
    }

showText :: Show a => a -> Text
showText = T.pack . show

getBinderName :: Binder -> Text
getBinderName Binder {..} = binderName
getBinderName TyBinder {..} = binderName

cvtExternalName :: CvtEnv -> GHCD.SExternalName -> ExternalName
cvtExternalName env GHCD.ExternalName {..} = ExternalName 
    { externalModuleName = GHCD.getModuleName externalModuleName
    , externalType = cvtType env externalType
    , .. 
    }

cvtBinder :: CvtEnv -> GHCD.SBinder -> Binder
cvtBinder env sbndr = case GHCD.unSBndr sbndr of
    GHCD.Binder {..} -> Binder
        { binderIdInfo = cvtIdInfo env binderIdInfo
        , binderType = cvtType env binderType
        , binderPhaseId = cvtEnvPhaseId env
        , ..
        }
    GHCD.TyBinder {..} -> TyBinder
        { binderKind = cvtType env binderKind
        , binderPhaseId = cvtEnvPhaseId env
        , ..
        }

cvtIdInfo :: CvtEnv -> GHCD.IdInfo GHCD.SBinder GHCD.BinderId -> IdInfo
cvtIdInfo env GHCD.IdInfo {..} = IdInfo
    { idiUnfolding = cvtUnfolding env idiUnfolding
    , ..
    }

cvtUnfolding :: CvtEnv -> GHCD.Unfolding GHCD.SBinder GHCD.BinderId -> Unfolding
cvtUnfolding env GHCD.NoUnfolding = NoUnfolding 
cvtUnfolding env GHCD.BootUnfolding = BootUnfolding
cvtUnfolding env (GHCD.OtherCon cons)  = OtherCon (map cvtAltCon cons)
cvtUnfolding env GHCD.DFunUnfolding = DFunUnfolding
cvtUnfolding env GHCD.CoreUnfolding {..} = CoreUnfolding
    { unfTemplate = cvtExpr env unfTemplate
    , ..
    }

cvtExpr :: CvtEnv -> GHCD.SExpr -> Expr
cvtExpr env (GHCD.EVar id) = EVar id
cvtExpr env (GHCD.EVarGlobal name) = EVarGlobal (cvtExternalName env name)
cvtExpr env (GHCD.ELit lit) = ELit (cvtLit lit)
cvtExpr env (GHCD.EApp f a) = EApp (cvtExpr env f) (cvtExpr env a)
cvtExpr env (GHCD.ETyLam bndr expr) = ETyLam (cvtBinder env bndr) (cvtExpr env expr)
cvtExpr env (GHCD.ELam bndr expr) = ELam (cvtBinder env bndr) (cvtExpr env expr)
cvtExpr env (GHCD.ELet bs body) = 
    let cvtPair (bndr, expr) = (cvtBinder env bndr, cvtExpr env expr)
    in ELet (map cvtPair bs) (cvtExpr env body)
cvtExpr env (GHCD.ECase expr bndr alts) = ECase (cvtExpr env expr) (cvtBinder env bndr) (map (cvtAlt env) alts)
cvtExpr env (GHCD.ETick t expr) = ETick t (cvtExpr env expr)
cvtExpr env (GHCD.EType t) = EType (cvtType env t)
cvtExpr env GHCD.ECoercion = ECoercion

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

cvtAlt :: CvtEnv -> GHCD.SAlt -> Alt
cvtAlt env GHCD.Alt {..} = Alt
    { altCon = cvtAltCon altCon
    , altBinders = map (cvtBinder env) altBinders
    , altRHS = cvtExpr env altRHS
    }

cvtAltCon :: GHCD.AltCon -> AltCon
cvtAltCon (GHCD.AltDataCon t) = AltDataCon t
cvtAltCon (GHCD.AltLit lit) = AltLit (cvtLit lit)
cvtAltCon (GHCD.AltDefault) = AltDefault

cvtType :: CvtEnv -> GHCD.SType -> Type
cvtType env (GHCD.VarTy id) = VarTy id
cvtType env (GHCD.FunTy f a) = FunTy (cvtType env f) (cvtType env a)
cvtType env (GHCD.TyConApp con ts) = TyConApp con (map (cvtType env) ts)
cvtType env (GHCD.AppTy f a) = AppTy (cvtType env f) (cvtType env a)
cvtType env (GHCD.ForAllTy bndr t) = ForAllTy (cvtBinder env bndr) (cvtType env t)
cvtType env (GHCD.LitTy) = LitTy
cvtType env (GHCD.CoercionTy) = CoercionTy

cvtTopBinding :: Text -> CvtEnv -> GHCD.STopBinding -> TopBinding
cvtTopBinding modname env tb = 
    let cvtInfo :: (GHCD.SBinder, CoreStats, GHCD.SExpr) -> TopBindingInfo
        cvtInfo (bndr, stats, expr) =
            let cvtedBinder = cvtBinder env bndr
                bndrName = getBinderName cvtedBinder

            in TopBindingInfo 
                { topBindingBinder = cvtedBinder
                , topBindingCoreState = stats
                , topBindingRHS = cvtExpr env expr
                , topBindingFromSource = False
                , topBindingIdx = Hash.hash (bndrName, modname)
                }
    in case tb of
         GHCD.NonRecTopBinding bndr stats expr -> NonRecTopBinding (cvtInfo (bndr, stats, expr))
         GHCD.RecTopBinding bs -> RecTopBinding (map cvtInfo bs)

cvtModule :: CvtEnv -> GHCD.SModule -> Module
cvtModule env GHCD.Module {..} = 
    let modName = GHCD.getModuleName moduleName
    in Module 
    { moduleName = modName
    , moduleTopBindings = map (cvtTopBinding modName env) moduleTopBindings
    , ..
    }
