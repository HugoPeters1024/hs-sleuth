module HsCore.Trafo.EraseTypes exposing (eraseTypesModule, eraseTypesTopBinding)

import Generated.Types exposing (..)
import HsCore.Helpers as H

eraseTypesModule : Module -> Module
eraseTypesModule mod = {mod | moduleTopBindings = List.map eraseTypesTopBinding mod.moduleTopBindings}

eraseTypesTopBinding : TopBinding -> TopBinding
eraseTypesTopBinding tp = 
    let go : TopBindingInfo -> TopBindingInfo
        go bi = {bi | topBindingRHS = eraseTypesExpr bi.topBindingRHS }
    in case tp of
        NonRecTopBinding bi -> NonRecTopBinding (go bi)
        RecTopBinding bis -> RecTopBinding (List.map go bis)

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
    EMarkDiff e -> EMarkDiff (eraseTypesExpr e)

eraseTypesAlt : Alt -> Alt
eraseTypesAlt alt = { alt | altBinders = List.filter (not << H.isTyBinder) alt.altBinders
                          , altRHS = eraseTypesExpr alt.altRHS }




