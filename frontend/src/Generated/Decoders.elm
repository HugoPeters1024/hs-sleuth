module Generated.Decoders exposing
    ( uniqueDecoder
    , externalNameDecoder
    , binderIdDecoder
    , binderDecoder
    , idInfoDecoder
    , unfoldingDecoder
    , occInfoDecoder
    , idDetailsDecoder
    , litDecoder
    , tyConDecoder
    , typeDecoder
    , moduleNameDecoder
    , moduleDecoder
    , exprDecoder
    , altDecoder
    , altConDecoder
    , lineColDecoder
    , srcSpanDecoder
    , tickDecoder
    , topBindingDecoder
    , coreStatsDecoder
    , moduleMetaDecoder
    , projectMetaDecoder
    , sessionMetaDecoder
    )

import Generated.Types exposing (..)
import Json.Decode
import Generated.Types exposing (..)
import Json.Decode.Pipeline


uniqueDecoder : Json.Decode.Decoder Unique
uniqueDecoder =
    Json.Decode.succeed Unique |>
    Json.Decode.Pipeline.custom (Json.Decode.index 0 (Json.Decode.string |>
    Json.Decode.andThen (\a -> case String.uncons a of
        Just (b , "") ->
            Json.Decode.succeed b

        _ ->
            Json.Decode.fail "Not a char"))) |>
    Json.Decode.Pipeline.custom (Json.Decode.index 1 Json.Decode.int)


externalNameDecoder : Json.Decode.Decoder ExternalName
externalNameDecoder =
    Json.Decode.field "tag" Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "ExternalName" ->
            Json.Decode.map ExternalName (Json.Decode.succeed (\b c d e -> { externalModuleName = b
            , externalName = c
            , externalUnique = d
            , externalType = e
            , localBinder = \_ -> Untouched}) |>
            Json.Decode.Pipeline.required "externalModuleName" moduleNameDecoder |>
            Json.Decode.Pipeline.required "externalName" Json.Decode.string |>
            Json.Decode.Pipeline.required "externalUnique" uniqueDecoder |>
            Json.Decode.Pipeline.required "externalType" typeDecoder)

        "ForeignCall" ->
            Json.Decode.succeed ForeignCall

        _ ->
            Json.Decode.fail "No matching constructor")


binderIdDecoder : Json.Decode.Decoder BinderId
binderIdDecoder =
    Json.Decode.map (\u -> BinderId u (\_ -> Untouched)) uniqueDecoder


binderDecoder : Json.Decode.Decoder Binder
binderDecoder =
    Json.Decode.field "tag" Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "Binder" ->
            Json.Decode.map Binder (Json.Decode.succeed (\b c d e f g -> { binderName = b
            , binderId = c
            , binderIdInfo = d
            , binderIdDetails = e
            , binderType = f
            , binderSrcSpan = g }) |>
            Json.Decode.Pipeline.required "binderName" Json.Decode.string |>
            Json.Decode.Pipeline.required "binderId" binderIdDecoder |>
            Json.Decode.Pipeline.required "binderIdInfo" idInfoDecoder |>
            Json.Decode.Pipeline.required "binderIdDetails" idDetailsDecoder |>
            Json.Decode.Pipeline.required "binderType" typeDecoder |>
            Json.Decode.Pipeline.required "binderSrcSpan" srcSpanDecoder)

        "TyBinder" ->
            Json.Decode.map TyBinder (Json.Decode.succeed (\b c d -> { binderName = b
            , binderId = c
            , binderKind = d }) |>
            Json.Decode.Pipeline.required "binderName" Json.Decode.string |>
            Json.Decode.Pipeline.required "binderId" binderIdDecoder |>
            Json.Decode.Pipeline.required "binderKind" typeDecoder)

        _ ->
            Json.Decode.fail "No matching constructor")


idInfoDecoder : Json.Decode.Decoder IdInfo
idInfoDecoder =
    Json.Decode.succeed IdInfo |>
    Json.Decode.Pipeline.required "idiArity" Json.Decode.int |>
    Json.Decode.Pipeline.required "idiIsOneShot" Json.Decode.bool |>
    Json.Decode.Pipeline.required "idiUnfolding" unfoldingDecoder |>
    Json.Decode.Pipeline.required "idiInlinePragma" Json.Decode.string |>
    Json.Decode.Pipeline.required "idiOccInfo" occInfoDecoder |>
    Json.Decode.Pipeline.required "idiStrictnessSig" Json.Decode.string |>
    Json.Decode.Pipeline.required "idiDemandSig" Json.Decode.string |>
    Json.Decode.Pipeline.required "idiCallArity" Json.Decode.int


unfoldingDecoder : Json.Decode.Decoder Unfolding
unfoldingDecoder =
    Json.Decode.field "tag" Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "NoUnfolding" ->
            Json.Decode.succeed NoUnfolding

        "BootUnfolding" ->
            Json.Decode.succeed BootUnfolding

        "OtherCon" ->
            Json.Decode.succeed OtherCon |>
            Json.Decode.Pipeline.required "contents" (Json.Decode.list altConDecoder)

        "DFunUnfolding" ->
            Json.Decode.succeed DFunUnfolding

        "CoreUnfolding" ->
            Json.Decode.map CoreUnfolding (Json.Decode.succeed (\b c d e f -> { unfTemplate = b
            , unfIsValue = c
            , unfIsConLike = d
            , unfIsWorkFree = e
            , unfGuidance = f }) |>
            Json.Decode.Pipeline.required "unfTemplate" exprDecoder |>
            Json.Decode.Pipeline.required "unfIsValue" Json.Decode.bool |>
            Json.Decode.Pipeline.required "unfIsConLike" Json.Decode.bool |>
            Json.Decode.Pipeline.required "unfIsWorkFree" Json.Decode.bool |>
            Json.Decode.Pipeline.required "unfGuidance" Json.Decode.string)

        _ ->
            Json.Decode.fail "No matching constructor")


occInfoDecoder : Json.Decode.Decoder OccInfo
occInfoDecoder =
    Json.Decode.field "tag" Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "OccManyOccs" ->
            Json.Decode.succeed OccManyOccs

        "OccDead" ->
            Json.Decode.succeed OccDead

        "OccOneOcc" ->
            Json.Decode.succeed OccOneOcc

        "OccLoopBreaker" ->
            Json.Decode.map OccLoopBreaker (Json.Decode.succeed (\b -> { occStrongLoopBreaker = b }) |>
            Json.Decode.Pipeline.required "occStrongLoopBreaker" Json.Decode.bool)

        _ ->
            Json.Decode.fail "No matching constructor")


idDetailsDecoder : Json.Decode.Decoder IdDetails
idDetailsDecoder =
    Json.Decode.field "tag" Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "VanillaId" ->
            Json.Decode.succeed VanillaId

        "RecSelId" ->
            Json.Decode.succeed RecSelId

        "DataConWorkId" ->
            Json.Decode.succeed DataConWorkId

        "DataConWrapId" ->
            Json.Decode.succeed DataConWrapId

        "ClassOpId" ->
            Json.Decode.succeed ClassOpId

        "PrimOpId" ->
            Json.Decode.succeed PrimOpId

        "TickBoxOpId" ->
            Json.Decode.succeed TickBoxOpId

        "DFunId" ->
            Json.Decode.succeed DFunId

        "CoVarId" ->
            Json.Decode.succeed CoVarId

        "JoinId" ->
            Json.Decode.map JoinId (Json.Decode.succeed (\b -> { joinIdArity = b }) |>
            Json.Decode.Pipeline.required "joinIdArity" Json.Decode.int)

        _ ->
            Json.Decode.fail "No matching constructor")


litDecoder : Json.Decode.Decoder Lit
litDecoder =
    Json.Decode.field "tag" Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "MachChar" ->
            Json.Decode.succeed MachChar |>
            Json.Decode.Pipeline.required "contents" (Json.Decode.string |>
            Json.Decode.andThen (\b -> case String.uncons b of
                Just (c , "") ->
                    Json.Decode.succeed c

                _ ->
                    Json.Decode.fail "Not a char"))

        "MachStr" ->
            Json.Decode.succeed MachStr |>
            Json.Decode.Pipeline.required "contents" Json.Decode.string

        "MachNullAddr" ->
            Json.Decode.succeed MachNullAddr

        "MachInt" ->
            Json.Decode.succeed MachInt |>
            Json.Decode.Pipeline.required "contents" Json.Decode.string

        "MachInt64" ->
            Json.Decode.succeed MachInt64 |>
            Json.Decode.Pipeline.required "contents" Json.Decode.string

        "MachWord" ->
            Json.Decode.succeed MachWord |>
            Json.Decode.Pipeline.required "contents" Json.Decode.string

        "MachWord64" ->
            Json.Decode.succeed MachWord64 |>
            Json.Decode.Pipeline.required "contents" Json.Decode.string

        "MachFloat" ->
            Json.Decode.succeed MachFloat |>
            Json.Decode.Pipeline.required "contents" Json.Decode.string

        "MachDouble" ->
            Json.Decode.succeed MachDouble |>
            Json.Decode.Pipeline.required "contents" Json.Decode.string

        "MachLabel" ->
            Json.Decode.succeed MachLabel |>
            Json.Decode.Pipeline.required "contents" Json.Decode.string

        "LitInteger" ->
            Json.Decode.succeed LitInteger |>
            Json.Decode.Pipeline.required "contents" Json.Decode.string

        "LitNatural" ->
            Json.Decode.succeed LitNatural |>
            Json.Decode.Pipeline.required "contents" Json.Decode.string

        "LitRubbish" ->
            Json.Decode.succeed LitRubbish

        _ ->
            Json.Decode.fail "No matching constructor")


tyConDecoder : Json.Decode.Decoder TyCon
tyConDecoder =
    Json.Decode.succeed TyCon |>
    Json.Decode.Pipeline.custom (Json.Decode.index 0 Json.Decode.string) |>
    Json.Decode.Pipeline.custom (Json.Decode.index 1 uniqueDecoder)


typeDecoder : Json.Decode.Decoder Type
typeDecoder =
    Json.Decode.field "tag" Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "VarTy" ->
            Json.Decode.succeed VarTy |>
            Json.Decode.Pipeline.required "contents" binderIdDecoder

        "FunTy" ->
            Json.Decode.field "contents" (Json.Decode.succeed FunTy |>
            Json.Decode.Pipeline.custom (Json.Decode.index 0 typeDecoder) |>
            Json.Decode.Pipeline.custom (Json.Decode.index 1 typeDecoder))

        "TyConApp" ->
            Json.Decode.field "contents" (Json.Decode.succeed TyConApp |>
            Json.Decode.Pipeline.custom (Json.Decode.index 0 tyConDecoder) |>
            Json.Decode.Pipeline.custom (Json.Decode.index 1 (Json.Decode.list typeDecoder)))

        "AppTy" ->
            Json.Decode.field "contents" (Json.Decode.succeed AppTy |>
            Json.Decode.Pipeline.custom (Json.Decode.index 0 typeDecoder) |>
            Json.Decode.Pipeline.custom (Json.Decode.index 1 typeDecoder))

        "ForAllTy" ->
            Json.Decode.field "contents" (Json.Decode.succeed ForAllTy |>
            Json.Decode.Pipeline.custom (Json.Decode.index 0 binderDecoder) |>
            Json.Decode.Pipeline.custom (Json.Decode.index 1 typeDecoder))

        "LitTy" ->
            Json.Decode.succeed LitTy

        "CoercionTy" ->
            Json.Decode.succeed CoercionTy

        _ ->
            Json.Decode.fail "No matching constructor")


moduleNameDecoder : Json.Decode.Decoder ModuleName
moduleNameDecoder =
    Json.Decode.succeed ModuleName |>
    Json.Decode.Pipeline.required "getModuleName" Json.Decode.string


moduleDecoder : Json.Decode.Decoder Module
moduleDecoder =
    Json.Decode.succeed Module |>
    Json.Decode.Pipeline.required "moduleName" moduleNameDecoder |>
    Json.Decode.Pipeline.required "modulePhase" Json.Decode.string |>
    Json.Decode.Pipeline.required "modulePhaseId" Json.Decode.int |>
    Json.Decode.Pipeline.required "moduleTopBindings" (Json.Decode.list topBindingDecoder)


exprDecoder : Json.Decode.Decoder Expr
exprDecoder =
    Json.Decode.field "tag" Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "EVar" ->
            Json.Decode.succeed EVar |>
            Json.Decode.Pipeline.required "contents" binderIdDecoder

        "EVarGlobal" ->
            Json.Decode.succeed EVarGlobal |>
            Json.Decode.Pipeline.required "contents" externalNameDecoder

        "ELit" ->
            Json.Decode.succeed ELit |>
            Json.Decode.Pipeline.required "contents" litDecoder

        "EApp" ->
            Json.Decode.field "contents" (Json.Decode.succeed EApp |>
            Json.Decode.Pipeline.custom (Json.Decode.index 0 exprDecoder) |>
            Json.Decode.Pipeline.custom (Json.Decode.index 1 exprDecoder))

        "ETyLam" ->
            Json.Decode.field "contents" (Json.Decode.succeed ETyLam |>
            Json.Decode.Pipeline.custom (Json.Decode.index 0 binderDecoder) |>
            Json.Decode.Pipeline.custom (Json.Decode.index 1 exprDecoder))

        "ELam" ->
            Json.Decode.field "contents" (Json.Decode.succeed ELam |>
            Json.Decode.Pipeline.custom (Json.Decode.index 0 binderDecoder) |>
            Json.Decode.Pipeline.custom (Json.Decode.index 1 exprDecoder))

        "ELet" ->
            Json.Decode.field "contents" (Json.Decode.succeed ELet |>
            Json.Decode.Pipeline.custom (Json.Decode.index 0 (Json.Decode.list (Json.Decode.map2 Tuple.pair (Json.Decode.index 0 binderDecoder) (Json.Decode.index 1 exprDecoder)))) |>
            Json.Decode.Pipeline.custom (Json.Decode.index 1 exprDecoder))

        "ECase" ->
            Json.Decode.field "contents" (Json.Decode.succeed ECase |>
            Json.Decode.Pipeline.custom (Json.Decode.index 0 exprDecoder) |>
            Json.Decode.Pipeline.custom (Json.Decode.index 1 binderDecoder) |>
            Json.Decode.Pipeline.custom (Json.Decode.index 2 (Json.Decode.list altDecoder)))

        "ETick" ->
            Json.Decode.field "contents" (Json.Decode.succeed ETick |>
            Json.Decode.Pipeline.custom (Json.Decode.index 0 tickDecoder) |>
            Json.Decode.Pipeline.custom (Json.Decode.index 1 exprDecoder))

        "EType" ->
            Json.Decode.succeed EType |>
            Json.Decode.Pipeline.required "contents" typeDecoder

        "ECoercion" ->
            Json.Decode.succeed ECoercion

        _ ->
            Json.Decode.fail "No matching constructor")


altDecoder : Json.Decode.Decoder Alt
altDecoder =
    Json.Decode.succeed Alt |>
    Json.Decode.Pipeline.required "altCon" altConDecoder |>
    Json.Decode.Pipeline.required "altBinders" (Json.Decode.list binderDecoder) |>
    Json.Decode.Pipeline.required "altRHS" exprDecoder


altConDecoder : Json.Decode.Decoder AltCon
altConDecoder =
    Json.Decode.field "tag" Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "AltDataCon" ->
            Json.Decode.succeed AltDataCon |>
            Json.Decode.Pipeline.required "contents" Json.Decode.string

        "AltLit" ->
            Json.Decode.succeed AltLit |>
            Json.Decode.Pipeline.required "contents" litDecoder

        "AltDefault" ->
            Json.Decode.succeed AltDefault

        _ ->
            Json.Decode.fail "No matching constructor")


lineColDecoder : Json.Decode.Decoder LineCol
lineColDecoder =
    Json.Decode.succeed LineCol |>
    Json.Decode.Pipeline.required "row" Json.Decode.int |>
    Json.Decode.Pipeline.required "column" Json.Decode.int


srcSpanDecoder : Json.Decode.Decoder SrcSpan
srcSpanDecoder =
    Json.Decode.field "tag" Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "SrcSpan" ->
            Json.Decode.map SrcSpan (Json.Decode.succeed (\b c d -> { spanFile = b
            , spanStart = c
            , spanEnd = d }) |>
            Json.Decode.Pipeline.required "spanFile" Json.Decode.string |>
            Json.Decode.Pipeline.required "spanStart" lineColDecoder |>
            Json.Decode.Pipeline.required "spanEnd" lineColDecoder)

        "NoSpan" ->
            Json.Decode.succeed NoSpan

        _ ->
            Json.Decode.fail "No matching constructor")


tickDecoder : Json.Decode.Decoder Tick
tickDecoder =
    Json.Decode.succeed Tick |>
    Json.Decode.Pipeline.required "sourceTickSpan" srcSpanDecoder


topBindingDecoder : Json.Decode.Decoder TopBinding
topBindingDecoder =
    Json.Decode.field "tag" Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "NonRecTopBinding" ->
            Json.Decode.field "contents" (Json.Decode.succeed NonRecTopBinding |>
            Json.Decode.Pipeline.custom (Json.Decode.index 0 binderDecoder) |>
            Json.Decode.Pipeline.custom (Json.Decode.index 1 coreStatsDecoder) |>
            Json.Decode.Pipeline.custom (Json.Decode.index 2 exprDecoder))

        "RecTopBinding" ->
            Json.Decode.succeed RecTopBinding |>
            Json.Decode.Pipeline.required "contents" (Json.Decode.list (Json.Decode.map3 triple (Json.Decode.index 0 binderDecoder) (Json.Decode.index 1 coreStatsDecoder) (Json.Decode.index 2 exprDecoder)))

        _ ->
            Json.Decode.fail "No matching constructor")


coreStatsDecoder : Json.Decode.Decoder CoreStats
coreStatsDecoder =
    Json.Decode.succeed CoreStats |>
    Json.Decode.Pipeline.required "csTerms" Json.Decode.int |>
    Json.Decode.Pipeline.required "csTypes" Json.Decode.int |>
    Json.Decode.Pipeline.required "csCoercions" Json.Decode.int |>
    Json.Decode.Pipeline.required "csValBinds" Json.Decode.int |>
    Json.Decode.Pipeline.required "csJoinBinds" Json.Decode.int


moduleMetaDecoder : Json.Decode.Decoder ModuleMeta
moduleMetaDecoder =
    Json.Decode.succeed ModuleMeta |>
    Json.Decode.Pipeline.required "nrPasses" Json.Decode.int |>
    Json.Decode.Pipeline.required "name" Json.Decode.string


projectMetaDecoder : Json.Decode.Decoder ProjectMeta
projectMetaDecoder =
    Json.Decode.succeed ProjectMeta |>
    Json.Decode.Pipeline.required "modules" (Json.Decode.list moduleMetaDecoder)


sessionMetaDecoder : Json.Decode.Decoder SessionMeta
sessionMetaDecoder =
    Json.Decode.succeed SessionMeta |>
    Json.Decode.Pipeline.required "sessions" (Json.Decode.list (Json.Decode.string))
triple : a -> b -> c -> (a,b,c)
triple x y z = (x,y,z)
