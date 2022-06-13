module HsCore.Trafo.Diff exposing (..)

import Generated.Types exposing (..)
import ElmHelpers as EH

annotateExpr : (Expr, Expr) -> (Expr, Expr)
annotateExpr inp = case inp of
    (EVar lhs, EVar rhs) -> if lhs.binderIdDeBruijn == rhs.binderIdDeBruijn then inp else mark inp
    (EVarGlobal _, EVarGlobal _) -> inp
    (ELit lhs, ELit rhs) -> if lhs == rhs then inp else mark inp
    (EApp f_lhs a_lhs, EApp f_rhs a_rhs) ->
        let (nf_lhs, nf_rhs) = annotateExpr (f_lhs, f_rhs)
            (na_lhs, na_rhs) = annotateExpr (a_lhs, a_rhs)
        in (EApp nf_lhs na_lhs, EApp nf_rhs na_rhs)
    (ETyLam b_lhs e_lhs, ETyLam b_rhs e_rhs) ->
        let (ne_lhs, ne_rhs) = annotateExpr (e_lhs, e_rhs)
        in (ETyLam b_lhs ne_lhs, ETyLam b_rhs ne_rhs)
    (ELam b_lhs e_lhs, ELam b_rhs e_rhs) ->
        let (ne_lhs, ne_rhs) = annotateExpr (e_lhs, e_rhs)
        in (ELam b_lhs ne_lhs, ELam b_rhs ne_rhs)
    (ELet bs_lhs e_lhs, ELet bs_rhs e_rhs) ->
        -- todo traverse rhs of bindings
        let (ne_lhs, ne_rhs) = annotateExpr (e_lhs, e_rhs)
        in (ELet bs_lhs ne_lhs, ELet bs_rhs ne_rhs)
    (ECase e_lhs b_lhs alts_lhs, ECase e_rhs b_rhs alts_rhs) ->
        let (ne_lhs, ne_rhs) = annotateExpr (e_lhs, e_rhs)
            (nalts_lhs, nalts_rhs) = List.unzip (List.map annotateAlt (EH.zip alts_lhs alts_rhs))
        in (ECase ne_lhs b_lhs nalts_lhs, ECase ne_rhs b_rhs nalts_rhs)
    ( ETick _ lhs, rhs ) -> annotateExpr (lhs, rhs)
    ( lhs, ETick _ rhs ) -> annotateExpr (lhs, rhs)
    ( EType lhs, EType rhs ) -> (EType lhs, EType rhs)
    ( ECoercion, ECoercion ) -> (ECoercion, ECoercion)
    (_,_)              -> mark inp

annotateAlt : (Alt, Alt) -> (Alt, Alt)
annotateAlt (lhs, rhs) = 
    let (ne_lhs, ne_rhs) = annotateExpr (lhs.altRHS, rhs.altRHS)
    in
    ( {lhs | altRHS = ne_lhs}
    , {rhs | altRHS = ne_rhs}
    )

anotateTopBindingInfo : (TopBindingInfo, TopBindingInfo) -> (TopBindingInfo, TopBindingInfo)
anotateTopBindingInfo (lhs, rhs) =
    let (e_lhs, e_rhs) = annotateExpr (lhs.topBindingRHS, rhs.topBindingRHS)
    in
    ( {lhs | topBindingRHS = e_lhs}
    , {rhs | topBindingRHS = e_rhs}
    )

mark : (Expr, Expr) -> (Expr, Expr)
mark (lhs, rhs) = (EMarkDiff lhs, EMarkDiff rhs)
