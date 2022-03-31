module Trafo exposing (..)

import Core.Generated.Types exposing (..)
import Dict exposing (Dict)

applyRenames : Dict String String -> CoreBind -> CoreBind
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
            Undef x -> Undef x

        tid : CoreId -> CoreId
        tid id = case Dict.get id.name r of
            Nothing -> id
            Just name -> {id | name = name}

        tbind : CoreBind -> CoreBind
        tbind (NonRec b e) = NonRec (tid b) (t e)

        talt : CoreAlt -> CoreAlt
        talt (Alt con bndrs e) = Alt con (List.map tid bndrs) (t e)
    in tbind




