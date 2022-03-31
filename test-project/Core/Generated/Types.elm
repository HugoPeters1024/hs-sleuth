module Core.Generated.Types exposing (..)

import Time exposing (Posix)


type alias CoreId =
    { name : String
    , id : Int
    }

type alias PassInfo =
    { idx : Int
    , title : String
    , binds : List CoreBind
    }

type CoreLiteral
    = CoreLitNumber String
    | CoreLitString String
    | CoreLitOther String

type CoreTerm
    = Var CoreId
    | Lit CoreLiteral
    | App CoreTerm CoreTerm
    | Lam CoreId CoreTerm
    | Let CoreBind CoreTerm
    | Case CoreTerm (List CoreAlt)
    | Type String
    | Undef String

type CoreBind
    = NonRec CoreId CoreTerm

type CoreAltCon
    = DataAlt String
    | LitAlt CoreLiteral
    | DEFAULT

type CoreAlt
    = Alt CoreAltCon (List CoreId) CoreTerm
