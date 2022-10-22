{-# LANGUAGE RecordWildCards #-}

module HsComprehension.Cvt where

import HsComprehension.Ast

import Data.Text (Text)
import qualified Data.Text as T

import Data.Hashable as Hash

import qualified GhcDump.Ast as GHCD
import HsComprehension.Hash (hashExprModAlpha)

data CvtEnv = CvtEnv
    { cvtEnvPhaseId :: Int
    , cvtEnvBinders :: [Int]
    }

binderUnique :: Binder -> Unique
binderUnique (Binder {..}) = binderIdUnique binderId
binderUnique (TyBinder {..}) = binderIdUnique binderId

envFindDeBruijn :: CvtEnv -> Unique -> Int
envFindDeBruijn env target =
    let go n [] = -1;
        go n (x:xs) = if x == target then n else go (n+1) xs
    in go 0 (cvtEnvBinders env)

envInsertBinderN :: [Unique] -> CvtEnv -> CvtEnv
envInsertBinderN bs env
  = foldl
      (\ env b -> env {cvtEnvBinders = b : cvtEnvBinders env}) env bs

envInsertBinder :: Unique -> CvtEnv -> CvtEnv
envInsertBinder x = envInsertBinderN [x]

showText :: Show a => a -> Text
showText = T.pack . show

getBinderName :: Binder -> Text
getBinderName Binder {..} = binderName
getBinderName TyBinder {..} = binderName

cvtUnique :: GHCD.Unique -> Unique
cvtUnique (GHCD.Unique _ i) = i

cvtTyCon :: GHCD.TyCon -> TyCon
cvtTyCon (GHCD.TyCon con u) = TyCon con (cvtUnique u)

cvtExternalName :: CvtEnv -> GHCD.SExternalName -> ExternalName
cvtExternalName env GHCD.ExternalName {..} = ExternalName
    { externalModuleName = GHCD.getModuleName externalModuleName
    , externalType = cvtType env externalType
    , externalUnique = cvtUnique externalUnique
    , ..
    }
cvtExternalName env GHCD.ForeignCall = ForeignCall

cvtBinder :: CvtEnv -> GHCD.SBinder -> Binder
cvtBinder env sbndr = case GHCD.unSBndr sbndr of
    GHCD.Binder {..} -> Binder
        { binderIdInfo = cvtIdInfo env binderIdInfo
        , binderId = cvtBinderId env binderId
        , binderType = cvtType env binderType
        , binderPhaseId = cvtEnvPhaseId env
        , binderCreatedPhaseId = 0
        , ..
        }
    GHCD.TyBinder {..} -> TyBinder
        { binderKind = cvtType env binderKind
        , binderId = cvtBinderId env binderId
        , binderPhaseId = cvtEnvPhaseId env
        , ..
        }

cvtBinderId :: CvtEnv -> GHCD.BinderId -> BinderId
cvtBinderId env (GHCD.BinderId unique) = BinderId
    { binderIdUnique = cvtUnique unique
    , binderIdRenderedUnique = T.pack $ show unique
    , binderIdDeBruijn = envFindDeBruijn env (cvtUnique unique)
    }

cvtIdInfo :: CvtEnv -> GHCD.IdInfo GHCD.SBinder GHCD.BinderId -> IdInfo
cvtIdInfo env GHCD.IdInfo {..} = 
-- to reduce the dump size, the details of the unfoldings care currently removed
  let removeUnfolding :: Unfolding -> Unfolding
      removeUnfolding u@CoreUnfolding {..} = u { unfTemplate = ELit (MachStr (T.pack "unfolding removed by plugin")) }
      removeUnfolding x = x
  in IdInfo
    { idiUnfolding = removeUnfolding $ cvtUnfolding env idiUnfolding
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
cvtExpr env (GHCD.EVar id) = EVar (cvtBinderId env id)
cvtExpr env (GHCD.EVarGlobal name) = EVarGlobal (cvtExternalName env name)
cvtExpr env (GHCD.ELit lit) = ELit (cvtLit lit)
cvtExpr env (GHCD.EApp f a) = EApp (cvtExpr env f) (cvtExpr env a)
cvtExpr env (GHCD.ETyLam bndr expr) = ETyLam (cvtBinder env bndr) (cvtExpr env expr)
cvtExpr env (GHCD.ELam bndr expr) =
    let cvtedBinder = cvtBinder env bndr
        env' = envInsertBinder (binderUnique cvtedBinder) env
     in ELam cvtedBinder (cvtExpr env' expr)
cvtExpr env (GHCD.ELet bs body) =
    let uniques = map ((\(GHCD.BinderId u) -> cvtUnique u) . GHCD.binderId . GHCD.unSBndr . fst) bs
        env' = envInsertBinderN uniques env
        cvtPair (bndr, expr) = (cvtBinder env' bndr, cvtExpr env' expr)
    in ELet (map cvtPair bs) (cvtExpr env' body)
cvtExpr env (GHCD.ECase expr bndr alts) = let
    cvtedBinder = cvtBinder env bndr
    env' = envInsertBinder (binderUnique cvtedBinder) env
   in ECase (cvtExpr env' expr) cvtedBinder (map (cvtAlt env') alts)
cvtExpr env (GHCD.ETick t expr) = ETick t (cvtExpr env expr)
cvtExpr env (GHCD.EType t) = EType (cvtType env t)
cvtExpr env GHCD.ECoercion = ECoercion

cvtLit :: GHCD.Lit -> Lit
cvtLit (GHCD.MachChar c) = MachChar c
cvtLit (GHCD.MachStr t) = MachStr (showText t)
cvtLit GHCD.MachNullAddr = MachNullAddr
cvtLit (GHCD.MachInt i) = MachInt (showText i)
cvtLit (GHCD.MachInt64 i) = MachInt64 (showText i)
cvtLit (GHCD.MachWord w) = MachWord (showText w)
cvtLit (GHCD.MachWord64 w) = MachWord64 (showText w)
cvtLit (GHCD.MachFloat f) = MachFloat (showText f)
cvtLit (GHCD.MachDouble d) = MachDouble (showText d)
cvtLit (GHCD.MachLabel l) = MachLabel l
cvtLit (GHCD.LitInteger i) = LitInteger (showText i)
cvtLit (GHCD.LitNatural n) = LitNatural (showText n)
cvtLit GHCD.LitRubbish = LitRubbish

cvtAlt :: CvtEnv -> GHCD.SAlt -> Alt
cvtAlt env GHCD.Alt {..} =
    let cvtedBinders = map (cvtBinder env) altBinders
        env' = envInsertBinderN (map binderUnique cvtedBinders) env
    in Alt
        { altCon = cvtAltCon altCon
        , altBinders = cvtedBinders
        , altRHS = cvtExpr env' altRHS
        }

cvtAltCon :: GHCD.AltCon -> AltCon
cvtAltCon (GHCD.AltDataCon t) = AltDataCon t
cvtAltCon (GHCD.AltLit lit) = AltLit (cvtLit lit)
cvtAltCon GHCD.AltDefault = AltDefault

cvtType :: CvtEnv -> GHCD.SType -> Type
cvtType env (GHCD.VarTy id) = VarTy (cvtBinderId env id)
cvtType env (GHCD.FunTy f a) = FunTy (cvtType env f) (cvtType env a)
cvtType env (GHCD.TyConApp con ts) = TyConApp (cvtTyCon con) (map (cvtType env) ts)
cvtType env (GHCD.AppTy f a) = AppTy (cvtType env f) (cvtType env a)
cvtType env (GHCD.ForAllTy bndr t) = ForAllTy (cvtBinder env bndr) (cvtType env t)
cvtType env (GHCD.LitTy tylit) = LitTy tylit
cvtType env GHCD.CoercionTy = CoercionTy

cvtTopBinding :: CvtEnv -> GHCD.STopBinding -> TopBinding
cvtTopBinding env tb =
    let cvtInfo :: (GHCD.SBinder, CoreStats, GHCD.SExpr) -> TopBindingInfo
        cvtInfo (bndr, stats, expr) =
            let cvtedBinder = cvtBinder env bndr
                bndrName = getBinderName cvtedBinder
                rhs = cvtExpr env expr

            in TopBindingInfo
                { topBindingBinder = cvtedBinder
                , topBindingCoreState = stats
                , topBindingRHS = rhs
                , topBindingFromSource = False
                , topBindingHash = hashExprModAlpha rhs
                }
    in case tb of
         GHCD.NonRecTopBinding bndr stats expr -> NonRecTopBinding (cvtInfo (bndr, stats, expr))
         GHCD.RecTopBinding bs -> RecTopBinding (map cvtInfo bs)

cvtPhase :: CvtEnv -> GHCD.SModule -> Phase
cvtPhase env GHCD.Module {..} = Phase
    { phaseName = modulePhase
    , phaseTopBindings = map (cvtTopBinding env) moduleTopBindings
    , phaseFiredRules = []
    , phaseId = modulePhaseId
    }
