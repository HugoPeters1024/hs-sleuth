module Core.Generated.Decoder exposing (..)

import Iso8601 as Iso
import Json.Decode as D exposing (..)
import Json.Decode.Pipeline as D exposing (required)

import Core.Generated.ElmStreet exposing (..)
import Core.Generated.Types as T


decodeMetaInfo : Decoder T.MetaInfo
decodeMetaInfo = D.succeed T.MetaInfo
    |> required "modules" (D.list D.string)

decodeCoreId : Decoder T.CoreId
decodeCoreId = D.succeed T.CoreId
    |> required "name" D.string
    |> required "uniquetag" D.string
    |> required "unique" D.int
    |> required "vartype" D.string
    |> required "istyvar" D.bool

decodePassInfo : Decoder T.PassInfo
decodePassInfo = D.succeed T.PassInfo
    |> required "idx" D.int
    |> required "title" D.string
    |> required "binds" (D.list decodeCoreBind)
    |> required "totalpasses" D.int
    |> required "modname" D.string

decodeCoreLiteral : Decoder T.CoreLiteral
decodeCoreLiteral =
    let decide : String -> Decoder T.CoreLiteral
        decide x = case x of
            "CoreLitNumber" -> D.field "contents" <| D.map T.CoreLitNumber D.string
            "CoreLitString" -> D.field "contents" <| D.map T.CoreLitString D.string
            "CoreLitOther" -> D.field "contents" <| D.map T.CoreLitOther D.string
            c -> D.fail <| "CoreLiteral doesn't have such constructor: " ++ c
    in D.andThen decide (D.field "tag" D.string)

decodeCoreTerm : Decoder T.CoreTerm
decodeCoreTerm =
    let decide : String -> Decoder T.CoreTerm
        decide x = case x of
            "Var" -> D.field "contents" <| D.map T.Var decodeCoreId
            "Lit" -> D.field "contents" <| D.map T.Lit decodeCoreLiteral
            "App" -> D.field "contents" <| D.map2 T.App (D.index 0 decodeCoreTerm) (D.index 1 decodeCoreTerm)
            "Lam" -> D.field "contents" <| D.map2 T.Lam (D.index 0 decodeCoreId) (D.index 1 decodeCoreTerm)
            "Let" -> D.field "contents" <| D.map2 T.Let (D.index 0 decodeCoreBind) (D.index 1 decodeCoreTerm)
            "Case" -> D.field "contents" <| D.map2 T.Case (D.index 0 decodeCoreTerm) (D.index 1 (D.list decodeCoreAlt))
            "Type" -> D.field "contents" <| D.map T.Type D.string
            "Undef" -> D.field "contents" <| D.map T.Undef D.string
            c -> D.fail <| "CoreTerm doesn't have such constructor: " ++ c
    in D.andThen decide (D.field "tag" D.string)

decodeCoreBind : Decoder T.CoreBind
decodeCoreBind =
    let decide : String -> Decoder T.CoreBind
        decide x = case x of
            "NonRec" -> D.field "contents" <| D.map2 T.NonRec (D.index 0 decodeCoreId) (D.index 1 decodeCoreTerm)
            c -> D.fail <| "CoreBind doesn't have such constructor: " ++ c
    in D.andThen decide (D.field "tag" D.string)

decodeCoreAltCon : Decoder T.CoreAltCon
decodeCoreAltCon =
    let decide : String -> Decoder T.CoreAltCon
        decide x = case x of
            "DataAlt" -> D.field "contents" <| D.map T.DataAlt D.string
            "LitAlt" -> D.field "contents" <| D.map T.LitAlt decodeCoreLiteral
            "DEFAULT" -> D.succeed T.DEFAULT
            c -> D.fail <| "CoreAltCon doesn't have such constructor: " ++ c
    in D.andThen decide (D.field "tag" D.string)

decodeCoreAlt : Decoder T.CoreAlt
decodeCoreAlt =
    let decide : String -> Decoder T.CoreAlt
        decide x = case x of
            "Alt" -> D.field "contents" <| D.map3 T.Alt (D.index 0 decodeCoreAltCon) (D.index 1 (D.list decodeCoreId)) (D.index 2 decodeCoreTerm)
            c -> D.fail <| "CoreAlt doesn't have such constructor: " ++ c
    in D.andThen decide (D.field "tag" D.string)
