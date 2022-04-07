module Core.Generated.Types exposing (..)

import Time exposing (Posix)


type alias MetaInfo =
    { modules : List String
    }

type alias CoreId =
    { name : String
    , uniquetag : String
    , unique : Int
    , vartype : String
    , istyvar : Bool
    }

type alias PassInfo =
    { idx : Int
    , title : String
    , binds : List CoreBind
    , srcbinders : List Int
    , totalpasses : Int
    , modname : String
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
    | Cast CoreTerm String
    | Coercion String

type CoreBind
    = NonRec CoreId CoreTerm

type CoreAltCon
    = DataAlt String
    | LitAlt CoreLiteral
    | DEFAULT

type CoreAlt
    = Alt CoreAltCon (List CoreId) CoreTerm
