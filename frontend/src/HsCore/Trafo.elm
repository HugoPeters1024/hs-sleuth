module HsCore.Trafo exposing (..)

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

trafoModule : Module -> Module
trafoModule mod = 
    let initialenv = withBindingN (H.getModuleBinders mod) Dict.empty
        newbinders = List.map (trafoTopBinding initialenv) mod.moduleTopBindings
        env = withBindingN (List.concatMap H.getTopLevelBinders newbinders) initialenv
    in { mod | moduleTopBindings = List.map (trafoTopBinding env) mod.moduleTopBindings }

trafoExternalName : Env -> ExternalName -> ExternalName
trafoExternalName env ename = case ename of
    ExternalName e -> ExternalName {e | externalType = trafoType env e.externalType, localBinder = lookupBinder env e.externalUnique}
    ForeignCall -> ForeignCall


trafoTopBinding : Env -> TopBinding -> TopBinding
trafoTopBinding env tb = case tb of
    -- TODO: also trafo the binding again for the unfolding
    NonRecTopBinding b stats expr -> NonRecTopBinding (trafoBinder env b) stats (trafoExpr env expr)
    RecTopBinding xs ->
        let (bs, stats, exprs) = H.unzip3 xs 
        in RecTopBinding <| H.zip3 (List.map (trafoBinder env) bs) stats (List.map (trafoExpr env) exprs)

trafoBinder : Env -> Binder -> Binder
trafoBinder env binder = case binder of
    Binder b -> Binder {b | binderType = trafoType env b.binderType}
    TyBinder b -> TyBinder {b | binderKind = trafoType env b.binderKind}

trafoExpr : Env -> Expr -> Expr
trafoExpr env expr = case expr of
    EVar (BinderId u _) -> EVar (BinderId u (lookupBinder env u))
    EVarGlobal n -> EVarGlobal (trafoExternalName env n)
    ELit l -> ELit l
    EApp f a -> EApp (trafoExpr env f) (trafoExpr env a)
    ETyLam b e -> 
        let nb = trafoBinder env b
            ne = trafoExpr (withBinding nb env) e
        in ETyLam nb ne
    ELam b e -> 
        let nb = trafoBinder env b
            ne = trafoExpr (withBinding nb env) e
        in ELam nb ne
    ELet bses e -> 
        let (bs, es) = List.unzip bses
            nbs = List.map (trafoBinder env) bs
            nenv = withBindingN nbs env
            nes = List.map (trafoExpr nenv) es
        in ELet (H.zip nbs nes) (trafoExpr nenv e)
    ECase e b alts -> 
        let nb = trafoBinder env b
            nenv = withBinding nb env 
            ne = trafoExpr nenv e
            nalts = List.map (trafoAlt nenv) alts
        in ECase ne nb nalts
    ETick t e -> ETick t (trafoExpr env e)
    EType t -> EType (trafoType env t)
    ECoercion -> ECoercion

trafoAlt : Env -> Alt -> Alt
trafoAlt env alt = case alt.altBinders of 
    [] -> { alt | altRHS = trafoExpr env alt.altRHS}
    x::xs -> 
        let nx = trafoBinder env x
            nenv = withBinding nx env
            falt = trafoAlt nenv {alt | altBinders = xs }
        in { falt | altBinders = nx :: falt.altBinders }

trafoType : Env -> Type -> Type
trafoType env type_ = case type_ of
    VarTy (BinderId u _) -> VarTy (BinderId u (lookupBinder env u))
    FunTy a b -> FunTy (trafoType env a) (trafoType env b)
    TyConApp con ts -> TyConApp con (List.map (trafoType env) ts)
    AppTy f a -> AppTy (trafoType env f) (trafoType env a)
    ForAllTy b t -> ForAllTy (trafoBinder env b) (trafoType (withBinding b env) t)
    LitTy -> LitTy
    CoercionTy -> CoercionTy








