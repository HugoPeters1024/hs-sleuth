module Core.Generated.Types exposing (..)

import Time exposing (Posix)


type CoreLiteral
    = CoreLitNumber String
    | CoreLitString String
    | CoreLitOther String

type CoreTerm
    = Var String
    | Lit CoreLiteral
    | App CoreTerm CoreTerm
    | Lam CoreBndr CoreTerm
    | Let CoreBind CoreTerm
    | Case CoreTerm (List CoreAlt)
    | Type String
    | Undef String

type CoreBind
    = NonRec CoreBndr CoreTerm

type alias CoreBndr =
    { name : String
    }

type CoreAltCon
    = DataAlt String
    | LitAlt CoreLiteral
    | DEFAULT

type CoreAlt
    = Alt CoreAltCon (List CoreBndr) CoreTerm
