module Generated.Types exposing
    ( ServerSettings
    , Capture
    , Unique(..)
    , ExternalName(..)
    , BinderId
    , Binder(..)
    , IdInfo
    , Unfolding(..)
    , OccInfo(..)
    , IdDetails(..)
    , Lit(..)
    , TyCon(..)
    , Type(..)
    , TyLit(..)
    , FiredRule
    , Phase
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



type alias ServerSettings  =
    { st_baseDir : String }


type alias Capture  =
    { captureName : String
    , captureDate : Int
    , captureGhcVersion : String
    , captureModules : List (String , Int) }


type Unique 
    = Unique Char Int


type ExternalName 
    = ExternalName { externalModuleName : String
    , externalName : String
    , externalUnique : Unique
    , externalType : Type
    , localBinder : BinderThunk }
    | ForeignCall 


type alias BinderId  =
    { binderIdThunk : BinderThunk,
    binderIdUnique : Unique
    , binderIdRenderedUnique : String
    , binderIdDeBruijn : Int }


type Binder 
    = Binder { binderName : String
    , binderId : BinderId
    , binderIdInfo : IdInfo
    , binderIdDetails : IdDetails
    , binderType : Type
    , binderSrcSpan : SrcSpan
    , binderPhaseId : Int
    , binderCreatedPhaseId : Int }
    | TyBinder { binderName : String
    , binderId : BinderId
    , binderKind : Type
    , binderPhaseId : Int }


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
    | LitTy TyLit
    | CoercionTy 


type TyLit 
    = NumTyLit Int
    | StrTyLit String
    | CharTyLit Char
    | UnknownLit 


type alias FiredRule  =
    { firedRuleName : String, firedRuleModule : String, firedRulePhase : Int }


type alias Phase  =
    { phaseName : String
    , phaseId : Int
    , phaseTopBindings : List TopBinding
    , phaseFiredRules : List FiredRule }


type alias Module  =
    { moduleName : String, modulePhases : List Phase }


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
    | EMarkDiff Expr


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
    , topBindingFromSource : Bool
    , topBindingHash : Int }


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
