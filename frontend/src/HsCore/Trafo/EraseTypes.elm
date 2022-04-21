module HsCore.Trafo.EraseTypes exposing (eraseTypesModule, eraseTypesTopBinding)

import Generated.Types exposing (..)
import HsCore.Helpers as H

mapMaybe : (a -> Maybe b) -> List a -> List b
mapMaybe f l = case l of
    [] -> []
    x::xs -> case f x of
        Just y -> y :: mapMaybe f xs
        Nothing -> mapMaybe f xs

eraseTypesModule : Module -> Module
eraseTypesModule mod = {mod | moduleTopBindings = mapMaybe (eraseTypesTopBinding) mod.moduleTopBindings}

eraseTypesTopBinding : TopBinding -> Maybe TopBinding
eraseTypesTopBinding tp = case tp of
    NonRecTopBinding b stats expr -> if H.isTyBinder b then Nothing else Just (NonRecTopBinding b stats (eraseTypesExpr expr))
    RecTopBinding bss -> 
        let (bs, stats, exprs) = H.unzip3 bss
        in Just (RecTopBinding (H.zip3 bs stats (List.map eraseTypesExpr exprs)))

eraseTypesExpr : Expr -> Expr
eraseTypesExpr expr = case expr of
    EVar x -> EVar x
    EVarGlobal g -> EVarGlobal g
    ELit l -> ELit l
    EApp f a -> case eraseTypesExpr a of
        EVar x -> if H.isTyBinderId x then (eraseTypesExpr f) else EApp (eraseTypesExpr f) (eraseTypesExpr a)
        EType _ -> eraseTypesExpr f
        _ -> EApp (eraseTypesExpr f) (eraseTypesExpr a)
    ETyLam _ a -> eraseTypesExpr a
    ELam b a -> ELam b (eraseTypesExpr a)
    -- TODO
    ELet bses e -> 
        let (bs, es) = List.unzip bses
        in ELet (H.zip bs (List.map eraseTypesExpr es)) (eraseTypesExpr e)
    ECase e b alts -> ECase (eraseTypesExpr e) b (List.map eraseTypesAlt alts)
    ETick t e -> ETick t (eraseTypesExpr e)
    EType t -> EType t
    ECoercion -> ECoercion

eraseTypesAlt : Alt -> Alt
eraseTypesAlt alt = { alt | altBinders = List.filter (not << H.isTyBinder) alt.altBinders
                          , altRHS = eraseTypesExpr alt.altRHS }




