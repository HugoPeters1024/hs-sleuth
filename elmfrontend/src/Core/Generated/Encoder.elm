module Core.Generated.Encoder exposing (..)

import Iso8601 as Iso
import Json.Encode as E exposing (..)

import Core.Generated.ElmStreet exposing (..)
import Core.Generated.Types as T


encodeMetaInfo : T.MetaInfo -> Value
encodeMetaInfo x = E.object
    [ ("tag", E.string "MetaInfo")
    , ("modules", (E.list E.string) x.modules)
    ]

encodeCoreId : T.CoreId -> Value
encodeCoreId x = E.object
    [ ("tag", E.string "CoreId")
    , ("name", E.string x.name)
    , ("uniquetag", E.string x.uniquetag)
    , ("unique", E.int x.unique)
    , ("vartype", E.string x.vartype)
    , ("istyvar", E.bool x.istyvar)
    ]

encodePassInfo : T.PassInfo -> Value
encodePassInfo x = E.object
    [ ("tag", E.string "PassInfo")
    , ("idx", E.int x.idx)
    , ("title", E.string x.title)
    , ("binds", (E.list encodeCoreBind) x.binds)
    , ("totalpasses", E.int x.totalpasses)
    , ("modname", E.string x.modname)
    ]

encodeCoreLiteral : T.CoreLiteral -> Value
encodeCoreLiteral x = E.object <| case x of
    T.CoreLitNumber x1 -> [("tag", E.string "CoreLitNumber"), ("contents", E.string x1)]
    T.CoreLitString x1 -> [("tag", E.string "CoreLitString"), ("contents", E.string x1)]
    T.CoreLitOther x1 -> [("tag", E.string "CoreLitOther"), ("contents", E.string x1)]

encodeCoreTerm : T.CoreTerm -> Value
encodeCoreTerm x = E.object <| case x of
    T.Var x1 -> [("tag", E.string "Var"), ("contents", encodeCoreId x1)]
    T.Lit x1 -> [("tag", E.string "Lit"), ("contents", encodeCoreLiteral x1)]
    T.App x1 x2 -> [("tag", E.string "App"), ("contents", E.list identity [encodeCoreTerm x1, encodeCoreTerm x2])]
    T.Lam x1 x2 -> [("tag", E.string "Lam"), ("contents", E.list identity [encodeCoreId x1, encodeCoreTerm x2])]
    T.Let x1 x2 -> [("tag", E.string "Let"), ("contents", E.list identity [encodeCoreBind x1, encodeCoreTerm x2])]
    T.Case x1 x2 -> [("tag", E.string "Case"), ("contents", E.list identity [encodeCoreTerm x1, (E.list encodeCoreAlt) x2])]
    T.Type x1 -> [("tag", E.string "Type"), ("contents", E.string x1)]
    T.Cast x1 x2 -> [("tag", E.string "Cast"), ("contents", E.list identity [encodeCoreTerm x1, E.string x2])]
    T.Coercion x1 -> [("tag", E.string "Coercion"), ("contents", E.string x1)]

encodeCoreBind : T.CoreBind -> Value
encodeCoreBind x = E.object <| case x of
    T.NonRec x1 x2 -> [("tag", E.string "NonRec"), ("contents", E.list identity [encodeCoreId x1, encodeCoreTerm x2])]

encodeCoreAltCon : T.CoreAltCon -> Value
encodeCoreAltCon x = E.object <| case x of
    T.DataAlt x1 -> [("tag", E.string "DataAlt"), ("contents", E.string x1)]
    T.LitAlt x1 -> [("tag", E.string "LitAlt"), ("contents", encodeCoreLiteral x1)]
    T.DEFAULT  -> [("tag", E.string "DEFAULT"), ("contents", E.list identity [])]

encodeCoreAlt : T.CoreAlt -> Value
encodeCoreAlt x = E.object <| case x of
    T.Alt x1 x2 x3 -> [("tag", E.string "Alt"), ("contents", E.list identity [encodeCoreAltCon x1, (E.list encodeCoreId) x2, encodeCoreTerm x3])]
