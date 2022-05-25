module Generated.Types exposing
    ( Capture
    , Unique(..)
    , ExternalName(..)
    , BinderId(..)
    , Binder(..)
    , IdInfo
    , Unfolding(..)
    , OccInfo(..)
    , IdDetails(..)
    , Lit(..)
    , TyCon(..)
    , Type(..)
    , Module
    , Expr(..)
    , Alt
    , AltCon(..)
    , LineCol
    , SrcSpan(..)
    , Tick
    , TopBindingInfo
    , TopBinding(..)
    , BinderThunk(..)
    , CoreStats
    )



type alias Capture  =
    { captureName : String
    , captureDate : Int
    , captureModules : List (String , Int) }


type Unique 
    = Unique Char Int


type ExternalName 
    = ExternalName { externalModuleName : String
    , externalName : String
    , externalUnique : Unique
    , externalType : Type
    , localBinder : () -> BinderThunk }
    | ForeignCall 


type BinderId 
    = BinderId Unique (() -> BinderThunk)


type Binder 
    = Binder { binderName : String
    , binderId : BinderId
    , binderIdInfo : IdInfo
    , binderIdDetails : IdDetails
    , binderType : Type
    , binderSrcSpan : SrcSpan }
    | TyBinder { binderName : String, binderId : BinderId, binderKind : Type }


type alias IdInfo  =
    { idiArity : Int
    , idiIsOneShot : Bool
    , idiUnfolding : Unfolding
    , idiInlinePragma : String
    , idiOccInfo : OccInfo
    , idiStrictnessSig : String
    , idiDemandSig : String
    , idiCallArity : Int }


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


type OccInfo 
    = OccManyOccs 
    | OccDead 
    | OccOneOcc 
    | OccLoopBreaker { occStrongLoopBreaker : Bool }


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


type TyCon 
    = TyCon String Unique


type Type 
    = VarTy BinderId
    | FunTy Type Type
    | TyConApp TyCon (List Type)
    | AppTy Type Type
    | ForAllTy Binder Type
    | LitTy 
    | CoercionTy 


type alias Module  =
    { moduleName : String
    , modulePhase : String
    , modulePhaseId : Int
    , moduleTopBindings : List TopBinding }


type Expr 
    = EVar BinderId
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


type alias Alt  =
    { altCon : AltCon, altBinders : List Binder, altRHS : Expr }


type AltCon 
    = AltDataCon String
    | AltLit Lit
    | AltDefault 


type alias LineCol  =
    { row : Int, column : Int }


type SrcSpan 
    = SrcSpan { spanFile : String, spanStart : LineCol, spanEnd : LineCol }
    | NoSpan 


type alias Tick  =
    { sourceTickSpan : SrcSpan }


type alias TopBindingInfo  =
    { topBindingBinder : Binder
    , topBindingCoreState : CoreStats
    , topBindingRHS : Expr
    , topBindingFromSource : Bool }


type TopBinding 
    = NonRecTopBinding TopBindingInfo
    | RecTopBinding (List TopBindingInfo)


type alias CoreStats  =
    { csTerms : Int
    , csTypes : Int
    , csCoercions : Int
    , csValBinds : Int
    , csJoinBinds : Int }
type BinderThunk = Found Binder | NotFound | Untouched
