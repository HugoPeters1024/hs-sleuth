module HsCore.Helpers exposing (..)

import ElmHelpers as EH

import Http
import Set exposing (Set)
import Char
import Generated.Types exposing (..)

type Var = VarBinder Binder
         | VarTop TopBindingInfo
         | VarExternal ExternalName

concatWith : String -> List String -> String
concatWith sep = String.concat << List.intersperse sep

resultToMaybe : Result x a -> Maybe a
resultToMaybe r = case r of
  Err _ -> Nothing
  Ok x -> Just x

concatSpaced : List String -> String
concatSpaced = concatWith " "

varIsTopLevel : Var -> Bool
varIsTopLevel var = case var of
    VarTop _ -> True
    _        -> False

varToInt : Var -> Int
varToInt term = case term of
    VarBinder b -> binderToInt b
    VarTop tb -> topBindingInfoToInt tb
    VarExternal e -> externalNameToInt e

varGHCUnique : Var -> String
varGHCUnique var = case var of
  VarBinder b -> (binderId b).binderIdRenderedUnique
  VarTop b -> (binderId b.topBindingBinder).binderIdRenderedUnique
  VarExternal (ExternalName e) -> uniqueToStr e.externalUnique
  _ -> ""

varPhaseId : Var -> Int
varPhaseId var = case var of
    VarBinder b -> binderPhaseId b
    VarTop b -> binderPhaseId b.topBindingBinder
    VarExternal _ -> -1

topBindingInfoToInt : TopBindingInfo -> Int
topBindingInfoToInt = binderToInt << .topBindingBinder

varIsConstructor : Var -> Bool
varIsConstructor = isConstructorName << varName

varName : Var -> String
varName var = case var of
    VarBinder b -> binderName b
    VarTop tb -> binderName tb.topBindingBinder
    VarExternal ext -> case ext of
        ExternalName e -> e.externalName
        _              -> "[ForeignCall]"

-- external names can refer to module local bindings
varExternalLocalBinder : Var -> Maybe Binder
varExternalLocalBinder var = case var of
    VarExternal (ExternalName e) -> case e.localBinder () of
        Found b -> Just b
        _       -> Nothing
    _                            -> Nothing

varType : Var -> Type
varType var = case var of
    VarBinder b -> binderType b
    VarTop b -> binderType b.topBindingBinder
    VarExternal ext -> case ext of
        ExternalName e -> e.externalType
        ForeignCall -> TyConApp (TyCon "ForeignCall" (Unique 't' -1)) []

binderName : Binder -> String
binderName binder = case binder of
    Binder b -> b.binderName
    TyBinder b -> b.binderName

binderId : Binder -> BinderId
binderId binder = case binder of
    Binder b -> b.binderId
    TyBinder b -> b.binderId

binderUpdateId : (BinderId -> BinderId) -> Binder -> Binder
binderUpdateId f binder = case binder of
    Binder b -> Binder {b | binderId = f b.binderId}
    TyBinder b -> TyBinder {b | binderId  = f b.binderId}


binderPhaseId : Binder -> Int
binderPhaseId binder = case binder of
    Binder b -> b.binderPhaseId
    TyBinder b -> b.binderPhaseId

binderUnique : Binder -> Unique
binderUnique binder = case binder of
    Binder b -> b.binderId.binderIdUnique
    TyBinder b -> b.binderId.binderIdUnique

binderUniqueStr : Binder -> String
binderUniqueStr = uniqueToStr << binderUnique

binderType : Binder -> Type
binderType binder = case binder of
    Binder b -> b.binderType
    TyBinder b -> b.binderKind

binderSpan : Binder -> SrcSpan
binderSpan bind = case bind of
    Binder b -> b.binderSrcSpan
    _        -> NoSpan

binderIsUnused : Binder -> Bool
binderIsUnused bndr = case bndr of
    TyBinder _ -> False
    Binder b -> case b.binderIdInfo.idiOccInfo of
        OccDead -> True
        _       -> case b.binderIdInfo.idiDemandSig of
            "A" -> True
            _   -> False

binderToInt : Binder -> Int
binderToInt = binderIdToInt << binderId

binderIdToInt : BinderId -> Int
binderIdToInt binderid = uniqueToInt binderid.binderIdUnique

uniqueToInt : Unique -> Int
uniqueToInt (Unique _ i) = i

uniqueToStr : Unique -> String
uniqueToStr (Unique _ i) = String.fromInt i

externalNameToInt : ExternalName -> Int
externalNameToInt en = case en of
    ExternalName n -> uniqueToInt n.externalUnique
    ForeignCall -> -1

isConstructorName : String -> Bool
isConstructorName name = case String.toList name of
    x::_ -> Char.isUpper x
    _    -> False

useFullSpan : SrcSpan -> Bool
useFullSpan span = case span of
    SrcSpan _ -> True
    NoSpan -> False

isTyBinder : Binder -> Bool
isTyBinder b = case b of
    Binder _ -> False
    TyBinder _ -> True

isTyBinderId : BinderId -> Bool
isTyBinderId binderid = case binderid.binderIdThunk () of
    Found b -> isTyBinder b
    _       -> False

-- Checks wether a list of alts contains  only the default case
-- This indicates a `seq` like usage and requires alternative printing
isOnlyDefaultAlt : List Alt -> Bool
isOnlyDefaultAlt alts = case alts of
    (alt::[]) -> isDefaultAlt alt
    _         -> False

isDefaultAlt : Alt -> Bool
isDefaultAlt alt = alt.altCon == AltDefault

leadingLambdas : Expr -> (Expr, List Binder)
leadingLambdas expr = case expr of
    ELam b e -> let (fe, bs) = leadingLambdas e in (fe, b::bs)
    ETyLam b e -> let (fe, bs) = leadingLambdas e in (fe, b::bs)
    _ -> (expr, [])

leadingForalls : Type -> (Type, List Binder)
leadingForalls type_ = case type_ of
    ForAllTy b t -> let (ft, bs) = leadingForalls t in (ft, b::bs)
    _            -> (type_, [])

leadingApps : Expr -> List Expr
leadingApps expr = case expr of
    EApp f a -> f::leadingApps a
    EMarkDiff e -> leadingApps e
    e -> [e]


getPhaseTopBinders : Phase -> List TopBindingInfo
getPhaseTopBinders mod = List.concatMap getTopLevelBinders mod.phaseTopBindings

unzip3 : List (a,b,c) -> (List a, List b, List c)
unzip3 xs = case xs of
    [] -> ([],[],[])
    ((a,b,c)::ys) -> let (ass, bs, cs) = unzip3 ys in (a::ass, b::bs, c::cs)

zip : List a -> List b -> List (a, b)
zip xs ys = case (xs, ys) of
    (x::xxs, y::yys) -> (x,y) :: zip xxs yys
    _                -> []

zip3 : List a -> List b -> List c -> (List (a, b, c))
zip3 xs ys zs = case (xs, ys, zs) of
    (x::xss, y::yss, z::zss) -> (x,y,z) :: zip3 xss yss zss
    _                        -> []

removeRecursiveGroups : List TopBinding -> List TopBinding
removeRecursiveGroups tbs =
    let go tb = case tb of
            NonRecTopBinding b -> [NonRecTopBinding b]
            RecTopBinding bs -> List.map NonRecTopBinding bs
    in List.concatMap go tbs

getTopLevelBinders : TopBinding -> List TopBindingInfo
getTopLevelBinders tp = case tp of
    NonRecTopBinding bi -> [bi]
    RecTopBinding bis -> bis

typeToStringParens : Type -> String
typeToStringParens type_ = case type_ of
    VarTy v -> typeToString (VarTy v)
    TyConApp con ts -> typeToString (TyConApp con ts)
    _ -> "(" ++ typeToString type_ ++ ")"



typeToString : Type -> String
typeToString type_ = case type_ of
    VarTy binderid -> case binderid.binderIdThunk () of
        Found x -> binderName x
        NotFound -> "[UKNOWN TYPEVAR]"
        Untouched -> "[TYPEVAR NEVER TRAVERSED]"
    FunTy x y -> typeToStringParens x ++ " -> " ++ typeToString y
    TyConApp (TyCon con _) ts -> 
        case ts of
            [] -> con
            _ -> let tsStr = concatSpaced (List.map typeToString ts)
                 in case con of
                    "[]" -> "[" ++ tsStr ++ "]"
                    _    -> con ++ " " ++ tsStr
    AppTy x y -> typeToString x ++ " " ++ typeToStringParens y
    ForAllTy b t -> 
        let (ft, bs) = leadingForalls t
            bndrsStr = String.concat <| List.intersperse " " (List.map binderName (b::bs))
        in "forall " ++ bndrsStr ++ ". " ++ typeToString ft
    LitTy tylit -> tyLitToString tylit
    CoercionTy -> "[CoercionTy]"

tyLitToString : TyLit -> String
tyLitToString tylit = case tylit of
  NumTyLit n -> String.fromInt n
  StrTyLit s -> s
  CharTyLit c -> String.fromChar c

topBindingMap : (TopBindingInfo -> TopBindingInfo) -> TopBinding -> TopBinding
topBindingMap f top = case top of
    NonRecTopBinding b -> NonRecTopBinding (f b)
    RecTopBinding bs -> RecTopBinding (List.map f bs)

exprTerms : Expr -> Int
exprTerms expr = case expr of
    EVar _ -> 1
    EVarGlobal _ -> 1
    ELit _ -> 1
    EApp f a -> exprTerms f + exprTerms a
    ETyLam _ _ -> 1
    ELam _ e -> 1 + exprTerms e
    ELet _ e -> 1 + exprTerms e
    ECase e _ alts -> 2 + exprTerms e + List.sum (List.map (exprTerms << .altRHS) alts)
    ETick _ e -> exprTerms e
    EType _ -> 1
    ECoercion -> 1
    EMarkDiff e -> exprTerms e

exprIsAtom : Expr -> Bool
exprIsAtom expr = case expr of
    EVar _ -> True
    EVarGlobal _ -> True
    ELit _ -> True
    ETick _ e -> exprIsAtom e
    EType _ -> True
    EMarkDiff e -> exprIsAtom e
    _ -> False

