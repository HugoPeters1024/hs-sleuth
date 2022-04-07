module Trafo exposing (..)

import Core.Generated.Types exposing (..)
import CoreLangUtils exposing (..)
import Dict exposing (Dict)
import Set
import Dict

applyRenames : Dict Int String -> CoreBind -> CoreBind
applyRenames r =
    let t : CoreTerm -> CoreTerm
        t b = case b of
            Var id -> Var (tid id)
            Lit i -> Lit i
            App e a -> App (t e) (t a)
            Lam bndr e -> Lam (tid bndr) (t e)
            Let bind e -> Let (tbind bind) (t e)
            Case e alts -> Case (t e) (List.map talt alts)
            Type tp -> Type tp
            Cast e c -> Cast (t e) c
            Coercion c -> Coercion c

        tid : CoreId -> CoreId
        tid id = case Dict.get id.unique r of
            Nothing -> id
            Just name -> {id | name = name}

        tbind : CoreBind -> CoreBind
        tbind (NonRec b e) = NonRec (tid b) (t e)

        talt : CoreAlt -> CoreAlt
        talt (Alt con bndrs e) = Alt con (List.map tid bndrs) (t e)
    in tbind


eraseTypes : CoreBind -> CoreBind
eraseTypes (NonRec b e) = NonRec b (eraseTypesTerm e)

eraseTypesTerm : CoreTerm -> CoreTerm
eraseTypesTerm term = case term of
    Var i -> Var i
    Lit i -> Lit i
    App e a -> case a of
        Type _ -> eraseTypesTerm e
        _      -> App (eraseTypesTerm e) (eraseTypesTerm a)
    Lam bndr e -> let ne = eraseTypesTerm e in if bndr.istyvar then ne else Lam bndr ne
    Let bind e -> Let (eraseTypes bind) (eraseTypesTerm e)
    Case e alts -> Case (eraseTypesTerm e) (List.map eraseTypeAlt alts)
    Type t -> Type t
    Cast e _ -> eraseTypesTerm e
    Coercion c -> Coercion c


eraseTypeAlt : CoreAlt -> CoreAlt
eraseTypeAlt (Alt con bs e) = Alt con (List.filter (\bndr -> not bndr.istyvar) bs) (eraseTypesTerm e)


collectAllVarsBind : CoreBind -> List CoreId
collectAllVarsBind (NonRec _ e) = collectAllVarTerm e

collectAllVarTerm : CoreTerm -> List CoreId
collectAllVarTerm term = case term of
    Var v -> [v]
    Lit _ -> []
    App e a -> collectAllVarTerm e ++ collectAllVarTerm a
    Lam _ e -> collectAllVarTerm e
    Let _ e -> collectAllVarTerm e
    Case e alts -> collectAllVarTerm e ++ List.concatMap collectAllVarsAlt alts
    Type _ -> []
    Cast e _ -> collectAllVarTerm e
    Coercion _ -> []

collectAllVarsAlt : CoreAlt -> List CoreId
collectAllVarsAlt (Alt _ _ e) = collectAllVarTerm e


leadingLambdas : CoreTerm -> (List CoreId, CoreTerm)
leadingLambdas term = case term of
    Lam b e -> let (bs, ne) = leadingLambdas e in (b::bs, ne)
    _ -> ([], term)


        

