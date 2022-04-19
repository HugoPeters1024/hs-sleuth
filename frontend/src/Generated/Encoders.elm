module Generated.Encoders exposing
    ( uniqueEncoder
    , externalNameEncoder
    , binderEncoder
    , idInfoEncoder
    , unfoldingEncoder
    , occInfoEncoder
    , idDetailsEncoder
    , litEncoder
    , tyConEncoder
    , typeEncoder
    , moduleEncoder
    , exprEncoder
    , altEncoder
    , altConEncoder
    , lineColEncoder
    , srcSpanEncoder
    , tickEncoder
    , coreStatsEncoder
    , topBinderEncoder
    , topBindingEncoder
    )

import Json.Encode
import Generated.Types exposing (..)


uniqueEncoder : Unique -> Json.Encode.Value
uniqueEncoder a =
    case a of
        Unique b c ->
            Json.Encode.list identity [ Json.Encode.string (String.fromChar b)
            , Json.Encode.int c ]


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


tyConEncoder : TyCon -> Json.Encode.Value
tyConEncoder a =
    case a of
        TyCon b c ->
            Json.Encode.list identity [Json.Encode.string b, uniqueEncoder c]


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


moduleEncoder : Module -> Json.Encode.Value
moduleEncoder a =
    Json.Encode.object [ ("moduleName" , Json.Encode.string a.moduleName)
    , ("modulePhase" , Json.Encode.string a.modulePhase)
    , ("modulePhaseId" , Json.Encode.int a.modulePhaseId)
    , ("moduleTopBindings" , Json.Encode.list topBindingEncoder a.moduleTopBindings) ]


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


altEncoder : Alt -> Json.Encode.Value
altEncoder a =
    Json.Encode.object [ ("altCon" , altConEncoder a.altCon)
    , ("altBinders" , Json.Encode.list binderEncoder a.altBinders)
    , ("altRHS" , exprEncoder a.altRHS) ]


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


lineColEncoder : LineCol -> Json.Encode.Value
lineColEncoder a =
    Json.Encode.object [ ("row" , Json.Encode.int a.row)
    , ("column" , Json.Encode.int a.column) ]


srcSpanEncoder : SrcSpan -> Json.Encode.Value
srcSpanEncoder a =
    Json.Encode.object [ ("spanFile" , Json.Encode.string a.spanFile)
    , ("spanStart" , lineColEncoder a.spanStart)
    , ("spanEnd" , lineColEncoder a.spanEnd) ]


tickEncoder : Tick -> Json.Encode.Value
tickEncoder a =
    Json.Encode.object [("sourceTickSpan" , srcSpanEncoder a.sourceTickSpan)]


coreStatsEncoder : CoreStats -> Json.Encode.Value
coreStatsEncoder a =
    Json.Encode.object [ ("csTerms" , Json.Encode.int a.csTerms)
    , ("csTypes" , Json.Encode.int a.csTypes)
    , ("csCoercions" , Json.Encode.int a.csCoercions)
    , ("csValBinds" , Json.Encode.int a.csValBinds)
    , ("csJoinBinds" , Json.Encode.int a.csJoinBinds) ]


topBinderEncoder : TopBinder -> Json.Encode.Value
topBinderEncoder a =
    case a of
        TopBinder b c d ->
            Json.Encode.list identity [ binderEncoder b
            , coreStatsEncoder c
            , exprEncoder d ]


topBindingEncoder : TopBinding -> Json.Encode.Value
topBindingEncoder a =
    case a of
        NonRecTopBinding b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "NonRecTopBinding")
            , ("contents" , topBinderEncoder b) ]

        RecTopBinding b ->
            Json.Encode.object [ ("tag" , Json.Encode.string "RecTopBinding")
            , ("contents" , Json.Encode.list topBinderEncoder b) ]
