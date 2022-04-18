module Generated.HsCore exposing
    ( Unique(..)
    , uniqueEncoder
    , uniqueDecoder
    , ExternalName(..)
    , externalNameEncoder
    , externalNameDecoder
    , Binder(..)
    , binderEncoder
    , binderDecoder
    , IdInfo
    , idInfoEncoder
    , idInfoDecoder
    , Unfolding(..)
    , unfoldingEncoder
    , unfoldingDecoder
    , OccInfo(..)
    , occInfoEncoder
    , occInfoDecoder
    , IdDetails(..)
    , idDetailsEncoder
    , idDetailsDecoder
    , Lit(..)
    , litEncoder
    , litDecoder
    , TyCon(..)
    , tyConEncoder
    , tyConDecoder
    , Type(..)
    , typeEncoder
    , typeDecoder
    , Module
    , moduleEncoder
    , moduleDecoder
    , Expr(..)
    , exprEncoder
    , exprDecoder
    , Alt
    , altEncoder
    , altDecoder
    , AltCon(..)
    , altConEncoder
    , altConDecoder
    , LineCol
    , lineColEncoder
    , lineColDecoder
    , SrcSpan
    , srcSpanEncoder
    , srcSpanDecoder
    , Tick
    , tickEncoder
    , tickDecoder
    , CoreStats
    , coreStatsEncoder
    , coreStatsDecoder
    , TopBinder(..)
    , topBinderEncoder
    , topBinderDecoder
    , TopBinding(..)
    , topBindingEncoder
    , topBindingDecoder
    )

import Json.Decode
import Json.Decode.Pipeline
import Json.Encode


type Unique 
    = Unique Char Int


uniqueEncoder : Unique -> Json.Encode.Value
uniqueEncoder a =
    case a of
        Unique b c ->
            Json.Encode.list identity [ Json.Encode.string (String.fromChar b)
            , Json.Encode.int c ]


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


type ExternalName 
    = ExternalName { externalModuleName : String
    , externalName : String
    , externalUnique : Unique
    , externalType : Type }
    | ForeignCall 


externalNameEncoder : ExternalName -> Json.Encode.Value
externalNameEncoder a =
    case a of
        ExternalName b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "ExternalName")
            , ("externalModuleName" , Json.Encode.string b.externalModuleName)
            , ("externalName" , Json.Encode.string b.externalName)
            , ("externalUnique" , uniqueEncoder b.externalUnique)
            , ("externalType" , typeEncoder b.externalType) ]

        ForeignCall ->
            Json.Encode.object [("tag" , Json.Encode.string "ForeignCall")]


externalNameDecoder : Json.Decode.Decoder ExternalName
externalNameDecoder =
    Json.Decode.field "tag" Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "ExternalName" ->
            Json.Decode.map ExternalName (Json.Decode.succeed (\b c d e -> { externalModuleName = b
            , externalName = c
            , externalUnique = d
            , externalType = e }) |>
            Json.Decode.Pipeline.required "externalModuleName" Json.Decode.string |>
            Json.Decode.Pipeline.required "externalName" Json.Decode.string |>
            Json.Decode.Pipeline.required "externalUnique" uniqueDecoder |>
            Json.Decode.Pipeline.required "externalType" typeDecoder)

        "ForeignCall" ->
            Json.Decode.succeed ForeignCall

        _ ->
            Json.Decode.fail "No matching constructor")


type Binder 
    = Binder { binderName : String
    , binderId : Unique
    , binderIdInfo : IdInfo
    , binderIdDetails : IdDetails
    , binderType : Type }
    | TyBinder { binderName : String, binderId : Unique, binderKind : Type }


binderEncoder : Binder -> Json.Encode.Value
binderEncoder a =
    case a of
        Binder b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "Binder")
            , ("binderName" , Json.Encode.string b.binderName)
            , ("binderId" , uniqueEncoder b.binderId)
            , ("binderIdInfo" , idInfoEncoder b.binderIdInfo)
            , ("binderIdDetails" , idDetailsEncoder b.binderIdDetails)
            , ("binderType" , typeEncoder b.binderType) ]

        TyBinder b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "TyBinder")
            , ("binderName" , Json.Encode.string b.binderName)
            , ("binderId" , uniqueEncoder b.binderId)
            , ("binderKind" , typeEncoder b.binderKind) ]


binderDecoder : Json.Decode.Decoder Binder
binderDecoder =
    Json.Decode.field "tag" Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "Binder" ->
            Json.Decode.map Binder (Json.Decode.succeed (\b c d e f -> { binderName = b
            , binderId = c
            , binderIdInfo = d
            , binderIdDetails = e
            , binderType = f }) |>
            Json.Decode.Pipeline.required "binderName" Json.Decode.string |>
            Json.Decode.Pipeline.required "binderId" uniqueDecoder |>
            Json.Decode.Pipeline.required "binderIdInfo" idInfoDecoder |>
            Json.Decode.Pipeline.required "binderIdDetails" idDetailsDecoder |>
            Json.Decode.Pipeline.required "binderType" typeDecoder)

        "TyBinder" ->
            Json.Decode.map TyBinder (Json.Decode.succeed (\b c d -> { binderName = b
            , binderId = c
            , binderKind = d }) |>
            Json.Decode.Pipeline.required "binderName" Json.Decode.string |>
            Json.Decode.Pipeline.required "binderId" uniqueDecoder |>
            Json.Decode.Pipeline.required "binderKind" typeDecoder)

        _ ->
            Json.Decode.fail "No matching constructor")


type alias IdInfo  =
    { idiArity : Int
    , idiIsOneShot : Bool
    , idiUnfolding : Unfolding
    , idiInlinePragma : String
    , idiOccInfo : OccInfo
    , idiStrictnessSig : String
    , idiDemandSig : String
    , idiCallArity : Int }


idInfoEncoder : IdInfo -> Json.Encode.Value
idInfoEncoder a =
    Json.Encode.object [ ("idiArity" , Json.Encode.int a.idiArity)
    , ("idiIsOneShot" , Json.Encode.bool a.idiIsOneShot)
    , ("idiUnfolding" , unfoldingEncoder a.idiUnfolding)
    , ("idiInlinePragma" , Json.Encode.string a.idiInlinePragma)
    , ("idiOccInfo" , occInfoEncoder a.idiOccInfo)
    , ("idiStrictnessSig" , Json.Encode.string a.idiStrictnessSig)
    , ("idiDemandSig" , Json.Encode.string a.idiDemandSig)
    , ("idiCallArity" , Json.Encode.int a.idiCallArity) ]


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


type Unfolding 
    = NoUnfolding 
    | BootUnfolding 
    | OtherCon (List AltCon)
    | DFunUnfolding 
    | CoreUnfolding { unfTemplate : Expr
    , unfIsValue : Bool
    , unfIsConLike : Bool
    , unfIsWorkFree : Bool
    , unfGuidance : String }


unfoldingEncoder : Unfolding -> Json.Encode.Value
unfoldingEncoder a =
    case a of
        NoUnfolding ->
            Json.Encode.object [("tag" , Json.Encode.string "NoUnfolding")]

        BootUnfolding ->
            Json.Encode.object [("tag" , Json.Encode.string "BootUnfolding")]

        OtherCon b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "OtherCon")
            , ("contents" , Json.Encode.list altConEncoder b) ]

        DFunUnfolding ->
            Json.Encode.object [("tag" , Json.Encode.string "DFunUnfolding")]

        CoreUnfolding b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "CoreUnfolding")
            , ("unfTemplate" , exprEncoder b.unfTemplate)
            , ("unfIsValue" , Json.Encode.bool b.unfIsValue)
            , ("unfIsConLike" , Json.Encode.bool b.unfIsConLike)
            , ("unfIsWorkFree" , Json.Encode.bool b.unfIsWorkFree)
            , ("unfGuidance" , Json.Encode.string b.unfGuidance) ]


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


type OccInfo 
    = OccManyOccs 
    | OccDead 
    | OccOneOcc 
    | OccLoopBreaker { occStrongLoopBreaker : Bool }


occInfoEncoder : OccInfo -> Json.Encode.Value
occInfoEncoder a =
    case a of
        OccManyOccs ->
            Json.Encode.object [("tag" , Json.Encode.string "OccManyOccs")]

        OccDead ->
            Json.Encode.object [("tag" , Json.Encode.string "OccDead")]

        OccOneOcc ->
            Json.Encode.object [("tag" , Json.Encode.string "OccOneOcc")]

        OccLoopBreaker b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "OccLoopBreaker")
            , ("occStrongLoopBreaker" , Json.Encode.bool b.occStrongLoopBreaker) ]


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


type IdDetails 
    = VanillaId 
    | RecSelId 
    | DataConWorkId 
    | DataConWrapId 
    | ClassOpId 
    | PrimOpId 
    | TickBoxOpId 
    | DFunId 
    | CoVarId 
    | JoinId { joinIdArity : Int }


idDetailsEncoder : IdDetails -> Json.Encode.Value
idDetailsEncoder a =
    case a of
        VanillaId ->
            Json.Encode.object [("tag" , Json.Encode.string "VanillaId")]

        RecSelId ->
            Json.Encode.object [("tag" , Json.Encode.string "RecSelId")]

        DataConWorkId ->
            Json.Encode.object [("tag" , Json.Encode.string "DataConWorkId")]

        DataConWrapId ->
            Json.Encode.object [("tag" , Json.Encode.string "DataConWrapId")]

        ClassOpId ->
            Json.Encode.object [("tag" , Json.Encode.string "ClassOpId")]

        PrimOpId ->
            Json.Encode.object [("tag" , Json.Encode.string "PrimOpId")]

        TickBoxOpId ->
            Json.Encode.object [("tag" , Json.Encode.string "TickBoxOpId")]

        DFunId ->
            Json.Encode.object [("tag" , Json.Encode.string "DFunId")]

        CoVarId ->
            Json.Encode.object [("tag" , Json.Encode.string "CoVarId")]

        JoinId b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "JoinId")
            , ("joinIdArity" , Json.Encode.int b.joinIdArity) ]


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


type Lit 
    = MachChar Char
    | MachStr String
    | MachNullAddr 
    | MachInt String
    | MachInt64 String
    | MachWord String
    | MachWord64 String
    | MachFloat String
    | MachDouble String
    | MachLabel String
    | LitInteger String
    | LitNatural String
    | LitRubbish 


litEncoder : Lit -> Json.Encode.Value
litEncoder a =
    case a of
        MachChar b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "MachChar")
            , ("contents" , Json.Encode.string (String.fromChar b)) ]

        MachStr b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "MachStr")
            , ("contents" , Json.Encode.string b) ]

        MachNullAddr ->
            Json.Encode.object [("tag" , Json.Encode.string "MachNullAddr")]

        MachInt b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "MachInt")
            , ("contents" , Json.Encode.string b) ]

        MachInt64 b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "MachInt64")
            , ("contents" , Json.Encode.string b) ]

        MachWord b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "MachWord")
            , ("contents" , Json.Encode.string b) ]

        MachWord64 b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "MachWord64")
            , ("contents" , Json.Encode.string b) ]

        MachFloat b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "MachFloat")
            , ("contents" , Json.Encode.string b) ]

        MachDouble b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "MachDouble")
            , ("contents" , Json.Encode.string b) ]

        MachLabel b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "MachLabel")
            , ("contents" , Json.Encode.string b) ]

        LitInteger b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "LitInteger")
            , ("contents" , Json.Encode.string b) ]

        LitNatural b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "LitNatural")
            , ("contents" , Json.Encode.string b) ]

        LitRubbish ->
            Json.Encode.object [("tag" , Json.Encode.string "LitRubbish")]


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


type TyCon 
    = TyCon String Unique


tyConEncoder : TyCon -> Json.Encode.Value
tyConEncoder a =
    case a of
        TyCon b c ->
            Json.Encode.list identity [Json.Encode.string b, uniqueEncoder c]


tyConDecoder : Json.Decode.Decoder TyCon
tyConDecoder =
    Json.Decode.succeed TyCon |>
    Json.Decode.Pipeline.custom (Json.Decode.index 0 Json.Decode.string) |>
    Json.Decode.Pipeline.custom (Json.Decode.index 1 uniqueDecoder)


type Type 
    = VarTy Unique
    | FunTy Type Type
    | TyConApp TyCon (List Type)
    | AppTy Type Type
    | ForAllTy Binder Type
    | LitTy 
    | CoercionTy 


typeEncoder : Type -> Json.Encode.Value
typeEncoder a =
    case a of
        VarTy b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "VarTy")
            , ("contents" , uniqueEncoder b) ]

        FunTy b c ->
            Json.Encode.object [ ("tag" , Json.Encode.string "FunTy")
            , ("contents" , Json.Encode.list identity [ typeEncoder b
            , typeEncoder c ]) ]

        TyConApp b c ->
            Json.Encode.object [ ("tag" , Json.Encode.string "TyConApp")
            , ("contents" , Json.Encode.list identity [ tyConEncoder b
            , Json.Encode.list typeEncoder c ]) ]

        AppTy b c ->
            Json.Encode.object [ ("tag" , Json.Encode.string "AppTy")
            , ("contents" , Json.Encode.list identity [ typeEncoder b
            , typeEncoder c ]) ]

        ForAllTy b c ->
            Json.Encode.object [ ("tag" , Json.Encode.string "ForAllTy")
            , ("contents" , Json.Encode.list identity [ binderEncoder b
            , typeEncoder c ]) ]

        LitTy ->
            Json.Encode.object [("tag" , Json.Encode.string "LitTy")]

        CoercionTy ->
            Json.Encode.object [("tag" , Json.Encode.string "CoercionTy")]


typeDecoder : Json.Decode.Decoder Type
typeDecoder =
    Json.Decode.field "tag" Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "VarTy" ->
            Json.Decode.succeed VarTy |>
            Json.Decode.Pipeline.required "contents" uniqueDecoder

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


type alias Module  =
    { moduleName : String
    , modulePhase : String
    , modulePhaseId : Int
    , moduleTopBindings : List TopBinding }


moduleEncoder : Module -> Json.Encode.Value
moduleEncoder a =
    Json.Encode.object [ ("moduleName" , Json.Encode.string a.moduleName)
    , ("modulePhase" , Json.Encode.string a.modulePhase)
    , ("modulePhaseId" , Json.Encode.int a.modulePhaseId)
    , ("moduleTopBindings" , Json.Encode.list topBindingEncoder a.moduleTopBindings) ]


moduleDecoder : Json.Decode.Decoder Module
moduleDecoder =
    Json.Decode.succeed Module |>
    Json.Decode.Pipeline.required "moduleName" Json.Decode.string |>
    Json.Decode.Pipeline.required "modulePhase" Json.Decode.string |>
    Json.Decode.Pipeline.required "modulePhaseId" Json.Decode.int |>
    Json.Decode.Pipeline.required "moduleTopBindings" (Json.Decode.list topBindingDecoder)


type Expr 
    = EVar Unique
    | EVarGlobal ExternalName
    | ELit Lit
    | EApp Expr Expr
    | ETyLam Binder Expr
    | ELam Binder Expr
    | ELet (List (Binder , Expr)) Expr
    | ECase Expr Binder (List Alt)
    | ETick Tick Expr
    | EType Type
    | ECoercion 


exprEncoder : Expr -> Json.Encode.Value
exprEncoder a =
    case a of
        EVar b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "EVar")
            , ("contents" , uniqueEncoder b) ]

        EVarGlobal b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "EVarGlobal")
            , ("contents" , externalNameEncoder b) ]

        ELit b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "ELit")
            , ("contents" , litEncoder b) ]

        EApp b c ->
            Json.Encode.object [ ("tag" , Json.Encode.string "EApp")
            , ("contents" , Json.Encode.list identity [ exprEncoder b
            , exprEncoder c ]) ]

        ETyLam b c ->
            Json.Encode.object [ ("tag" , Json.Encode.string "ETyLam")
            , ("contents" , Json.Encode.list identity [ binderEncoder b
            , exprEncoder c ]) ]

        ELam b c ->
            Json.Encode.object [ ("tag" , Json.Encode.string "ELam")
            , ("contents" , Json.Encode.list identity [ binderEncoder b
            , exprEncoder c ]) ]

        ELet b c ->
            Json.Encode.object [ ("tag" , Json.Encode.string "ELet")
            , ("contents" , Json.Encode.list identity [ Json.Encode.list (\d -> case d of
                (e , f) ->
                    Json.Encode.list identity [ binderEncoder e
                    , exprEncoder f ]) b
            , exprEncoder c ]) ]

        ECase b c d ->
            Json.Encode.object [ ("tag" , Json.Encode.string "ECase")
            , ("contents" , Json.Encode.list identity [ exprEncoder b
            , binderEncoder c
            , Json.Encode.list altEncoder d ]) ]

        ETick b c ->
            Json.Encode.object [ ("tag" , Json.Encode.string "ETick")
            , ("contents" , Json.Encode.list identity [ tickEncoder b
            , exprEncoder c ]) ]

        EType b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "EType")
            , ("contents" , typeEncoder b) ]

        ECoercion ->
            Json.Encode.object [("tag" , Json.Encode.string "ECoercion")]


exprDecoder : Json.Decode.Decoder Expr
exprDecoder =
    Json.Decode.field "tag" Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "EVar" ->
            Json.Decode.succeed EVar |>
            Json.Decode.Pipeline.required "contents" uniqueDecoder

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


type alias Alt  =
    { altCon : AltCon, altBinders : List Binder, altRHS : Expr }


altEncoder : Alt -> Json.Encode.Value
altEncoder a =
    Json.Encode.object [ ("altCon" , altConEncoder a.altCon)
    , ("altBinders" , Json.Encode.list binderEncoder a.altBinders)
    , ("altRHS" , exprEncoder a.altRHS) ]


altDecoder : Json.Decode.Decoder Alt
altDecoder =
    Json.Decode.succeed Alt |>
    Json.Decode.Pipeline.required "altCon" altConDecoder |>
    Json.Decode.Pipeline.required "altBinders" (Json.Decode.list binderDecoder) |>
    Json.Decode.Pipeline.required "altRHS" exprDecoder


type AltCon 
    = AltDataCon String
    | AltLit Lit
    | AltDefault 


altConEncoder : AltCon -> Json.Encode.Value
altConEncoder a =
    case a of
        AltDataCon b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "AltDataCon")
            , ("contents" , Json.Encode.string b) ]

        AltLit b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "AltLit")
            , ("contents" , litEncoder b) ]

        AltDefault ->
            Json.Encode.object [("tag" , Json.Encode.string "AltDefault")]


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


type alias LineCol  =
    { row : Int, column : Int }


lineColEncoder : LineCol -> Json.Encode.Value
lineColEncoder a =
    Json.Encode.object [ ("row" , Json.Encode.int a.row)
    , ("column" , Json.Encode.int a.column) ]


lineColDecoder : Json.Decode.Decoder LineCol
lineColDecoder =
    Json.Decode.succeed LineCol |>
    Json.Decode.Pipeline.required "row" Json.Decode.int |>
    Json.Decode.Pipeline.required "column" Json.Decode.int


type alias SrcSpan  =
    { spanFile : String, spanStart : LineCol, spanEnd : LineCol }


srcSpanEncoder : SrcSpan -> Json.Encode.Value
srcSpanEncoder a =
    Json.Encode.object [ ("spanFile" , Json.Encode.string a.spanFile)
    , ("spanStart" , lineColEncoder a.spanStart)
    , ("spanEnd" , lineColEncoder a.spanEnd) ]


srcSpanDecoder : Json.Decode.Decoder SrcSpan
srcSpanDecoder =
    Json.Decode.succeed SrcSpan |>
    Json.Decode.Pipeline.required "spanFile" Json.Decode.string |>
    Json.Decode.Pipeline.required "spanStart" lineColDecoder |>
    Json.Decode.Pipeline.required "spanEnd" lineColDecoder


type alias Tick  =
    { sourceTickSpan : SrcSpan }


tickEncoder : Tick -> Json.Encode.Value
tickEncoder a =
    Json.Encode.object [("sourceTickSpan" , srcSpanEncoder a.sourceTickSpan)]


tickDecoder : Json.Decode.Decoder Tick
tickDecoder =
    Json.Decode.succeed Tick |>
    Json.Decode.Pipeline.required "sourceTickSpan" srcSpanDecoder


type alias CoreStats  =
    { csTerms : Int
    , csTypes : Int
    , csCoercions : Int
    , csValBinds : Int
    , csJoinBinds : Int }


coreStatsEncoder : CoreStats -> Json.Encode.Value
coreStatsEncoder a =
    Json.Encode.object [ ("csTerms" , Json.Encode.int a.csTerms)
    , ("csTypes" , Json.Encode.int a.csTypes)
    , ("csCoercions" , Json.Encode.int a.csCoercions)
    , ("csValBinds" , Json.Encode.int a.csValBinds)
    , ("csJoinBinds" , Json.Encode.int a.csJoinBinds) ]


coreStatsDecoder : Json.Decode.Decoder CoreStats
coreStatsDecoder =
    Json.Decode.succeed CoreStats |>
    Json.Decode.Pipeline.required "csTerms" Json.Decode.int |>
    Json.Decode.Pipeline.required "csTypes" Json.Decode.int |>
    Json.Decode.Pipeline.required "csCoercions" Json.Decode.int |>
    Json.Decode.Pipeline.required "csValBinds" Json.Decode.int |>
    Json.Decode.Pipeline.required "csJoinBinds" Json.Decode.int


type TopBinder 
    = TopBinder Binder CoreStats Expr


topBinderEncoder : TopBinder -> Json.Encode.Value
topBinderEncoder a =
    case a of
        TopBinder b c d ->
            Json.Encode.list identity [ binderEncoder b
            , coreStatsEncoder c
            , exprEncoder d ]


topBinderDecoder : Json.Decode.Decoder TopBinder
topBinderDecoder =
    Json.Decode.succeed TopBinder |>
    Json.Decode.Pipeline.custom (Json.Decode.index 0 binderDecoder) |>
    Json.Decode.Pipeline.custom (Json.Decode.index 1 coreStatsDecoder) |>
    Json.Decode.Pipeline.custom (Json.Decode.index 2 exprDecoder)


type TopBinding 
    = NonRecTopBinding TopBinder
    | RecTopBinding (List TopBinder)


topBindingEncoder : TopBinding -> Json.Encode.Value
topBindingEncoder a =
    case a of
        NonRecTopBinding b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "NonRecTopBinding")
            , ("contents" , topBinderEncoder b) ]

        RecTopBinding b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "RecTopBinding")
            , ("contents" , Json.Encode.list topBinderEncoder b) ]


topBindingDecoder : Json.Decode.Decoder TopBinding
topBindingDecoder =
    Json.Decode.field "tag" Json.Decode.string |>
    Json.Decode.andThen (\a -> case a of
        "NonRecTopBinding" ->
            Json.Decode.succeed NonRecTopBinding |>
            Json.Decode.Pipeline.required "contents" topBinderDecoder

        "RecTopBinding" ->
            Json.Decode.succeed RecTopBinding |>
            Json.Decode.Pipeline.required "contents" (Json.Decode.list topBinderDecoder)

        _ ->
            Json.Decode.fail "No matching constructor")
