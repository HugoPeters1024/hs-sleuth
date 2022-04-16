module HsCore.Trafo exposing (..)

import Generated.HsCore exposing (..)
import HsCore.Helpers exposing (..)

eraseTypesModule : Module -> Module
eraseTypesModule mod = { mod | moduleTopBindings = List.map eraseTypesTopBinding mod.moduleTopBindings }

eraseTypesTopBinding : TopBinding -> TopBinding
eraseTypesTopBinding b = case b of
    NonRecTopBinding tb -> NonRecTopBinding (eraseTypesTopBinder tb)
    RecTopBinding tbs -> RecTopBinding (List.map eraseTypesTopBinder tbs)

eraseTypesTopBinder : TopBinder -> TopBinder
eraseTypesTopBinder (TopBinder b s e) = TopBinder b s (eraseTypesExpr e)

eraseTypesExpr : Expr -> Expr
eraseTypesExpr expr = case expr of
    EVar b -> EVar b
    EVarGlobal b -> EVarGlobal b
    ELit lit -> ELit lit
    EApp e a -> case a of
        EType _ -> eraseTypesExpr e
        _ -> EApp (eraseTypesExpr e) (eraseTypesExpr a)
    ETyLam _ e -> eraseTypesExpr e
    ELam b e -> ELam b (eraseTypesExpr e)
    ELet bs e -> ELet bs (eraseTypesExpr e)
    ECase e b alts -> ECase (eraseTypesExpr e) b (List.map eraseTypesAlt alts)
    ETick t e -> ETick t (eraseTypesExpr e)
    EType t -> EType t
    ECoercion -> ECoercion

eraseTypesAlt : Alt -> Alt
eraseTypesAlt alt = {alt | altRHS = eraseTypesExpr alt.altRHS}






