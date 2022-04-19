module Generated.Types exposing
    ( Unique(..)
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
    , ModuleName
    , Module
    , Expr(..)
    , Alt
    , AltCon(..)
    , LineCol
    , SrcSpan
    , Tick
    , TopBinding(..)
    , CoreStats
    )



type Unique 
    = Unique Char Int


type ExternalName 
    = ExternalName { externalModuleName : ModuleName
    , externalName : String
    , externalUnique : Unique
    , externalType : Type }
    | ForeignCall 


type BinderId 
    = BinderId Unique


type Binder 
    = Binder { binderName : String
    , binderId : BinderId
    , binderIdInfo : IdInfo
    , binderIdDetails : IdDetails
    , binderType : Type }
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


type alias ModuleName  =
    { getModuleName : String }


type alias Module  =
    { moduleName : ModuleName
    , modulePhase : String
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


type alias SrcSpan  =
    { spanFile : String, spanStart : LineCol, spanEnd : LineCol }


type alias Tick  =
    { sourceTickSpan : SrcSpan }


type TopBinding 
    = NonRecTopBinding Binder CoreStats Expr
    | RecTopBinding (List (Binder, CoreStats, Expr))


type alias CoreStats  =
    { csTerms : Int
    , csTypes : Int
    , csCoercions : Int
    , csValBinds : Int
    , csJoinBinds : Int }
