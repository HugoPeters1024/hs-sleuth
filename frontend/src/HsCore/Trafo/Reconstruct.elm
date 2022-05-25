module HsCore.Trafo.Reconstruct exposing (reconModule)

import Generated.Types exposing (..)
import HsCore.Helpers as H exposing (..)

import Dict exposing (Dict)

type alias Env = Dict Int Binder

withBinding : Binder -> Env -> Env
withBinding bndr env = Dict.insert (H.binderToInt bndr) bndr env

withBindingN : List Binder -> Env -> Env
withBindingN bs env = List.foldl withBinding env bs

lookupBinder : Env -> Unique -> (() -> BinderThunk)
lookupBinder env u = \_ -> case Dict.get (H.uniqueToInt u) env of
    Just x -> Found x
    Nothing -> NotFound

reconModule : Module -> Module
reconModule mod = 
    let initialenv = withBindingN (List.map .topBindingBinder (H.getModuleTopBinders mod)) Dict.empty
        newbinders = List.map (reconTopBinding initialenv) mod.moduleTopBindings
        env = withBindingN (List.concatMap (List.map .topBindingBinder << H.getTopLevelBinders) newbinders) initialenv
    in { mod | moduleTopBindings = List.map (reconTopBinding env) mod.moduleTopBindings }

reconExternalName : Env -> ExternalName -> ExternalName
reconExternalName env ename = case ename of
    ExternalName e -> ExternalName {e | externalType = reconType env e.externalType, localBinder = lookupBinder env e.externalUnique}
    ForeignCall -> ForeignCall


reconTopBinding : Env -> TopBinding -> TopBinding
reconTopBinding env tb = 
    let go : TopBindingInfo -> TopBindingInfo 
        go bi = { bi | topBindingBinder = reconBinder env bi.topBindingBinder 
                     , topBindingRHS = reconExpr env bi.topBindingRHS
                }
    in case tb of
        -- TODO: also recon the binding again for the unfolding
        NonRecTopBinding bi -> NonRecTopBinding (go bi)
        RecTopBinding bis -> RecTopBinding (List.map go bis)

reconBinder : Env -> Binder -> Binder
reconBinder env binder = case binder of
    Binder b -> Binder {b | binderType = reconType env b.binderType}
    TyBinder b -> TyBinder {b | binderKind = reconType env b.binderKind}

reconExpr : Env -> Expr -> Expr
reconExpr env expr = case expr of
    EVar (BinderId u _) -> EVar (BinderId u (lookupBinder env u))
    EVarGlobal n -> EVarGlobal (reconExternalName env n)
    ELit l -> ELit l
    EApp f a -> EApp (reconExpr env f) (reconExpr env a)
    ETyLam b e -> 
        let nb = reconBinder env b
            ne = reconExpr (withBinding nb env) e
        in ETyLam nb ne
    ELam b e -> 
        let nb = reconBinder env b
            ne = reconExpr (withBinding nb env) e
        in ELam nb ne
    ELet bses e -> 
        let (bs, es) = List.unzip bses
            nbs = List.map (reconBinder env) bs
            nenv = withBindingN nbs env
            nes = List.map (reconExpr nenv) es
        in ELet (H.zip nbs nes) (reconExpr nenv e)
    ECase e b alts -> 
        let nb = reconBinder env b
            nenv = withBinding nb env 
            ne = reconExpr nenv e
            nalts = List.map (reconAlt nenv) alts
        in ECase ne nb nalts
    ETick t e -> ETick t (reconExpr env e)
    EType t -> EType (reconType env t)
    ECoercion -> ECoercion

reconAlt : Env -> Alt -> Alt
reconAlt env alt = case alt.altBinders of 
    [] -> { alt | altRHS = reconExpr env alt.altRHS}
    x::xs -> 
        let nx = reconBinder env x
            nenv = withBinding nx env
            falt = reconAlt nenv {alt | altBinders = xs }
        in { falt | altBinders = nx :: falt.altBinders }

reconType : Env -> Type -> Type
reconType env type_ = case type_ of
    VarTy (BinderId u _) -> VarTy (BinderId u (lookupBinder env u))
    FunTy a b -> FunTy (reconType env a) (reconType env b)
    TyConApp con ts -> TyConApp con (List.map (reconType env) ts)
    AppTy f a -> AppTy (reconType env f) (reconType env a)
    ForAllTy b t -> ForAllTy (reconBinder env b) (reconType (withBinding b env) t)
    LitTy -> LitTy
    CoercionTy -> CoercionTy








