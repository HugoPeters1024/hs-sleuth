module HsCore.Helpers exposing (..)

import Char

import Generated.Types exposing (..)

concatWith : String -> List String -> String
concatWith sep = String.concat << List.intersperse sep

concatSpaced : List String -> String
concatSpaced = concatWith " "

binderName : Binder -> String
binderName binder = case binder of
    Binder b -> b.binderName
    TyBinder b -> b.binderName

binderId : Binder -> BinderId
binderId binder = case binder of
    Binder b -> b.binderId
    TyBinder b -> b.binderId

binderUnique : Binder -> Unique
binderUnique b = let (BinderId u _) = binderId b in u

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

externalName : ExternalName -> String
externalName en = case en of
    ExternalName n -> n.externalName
    ForeignCall -> "ForeignCall"

binderToInt : Binder -> Int
binderToInt = binderIdToInt << binderId

binderIdToInt : BinderId -> Int
binderIdToInt (BinderId u _) = uniqueToInt u

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

isSrcBinder : Binder -> Bool
isSrcBinder binder = case binder of
    Binder b -> useFullSpan b.binderSrcSpan && not (String.startsWith "$" b.binderName)
    _ -> False

isTyBinder : Binder -> Bool
isTyBinder b = case b of
    Binder _ -> False
    TyBinder _ -> True

isTyBinderId : BinderId -> Bool
isTyBinderId (BinderId _ getBinder) = case getBinder () of
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


getModuleBinders : Module -> List Binder
getModuleBinders mod = List.concatMap getTopLevelBinders mod.moduleTopBindings

getModuleTopNames : Module -> List (String, Bool)
getModuleTopNames mod = List.map (\b -> (binderName b, isSrcBinder b)) (getModuleBinders mod)

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

getTopLevelBinders : TopBinding -> List Binder
getTopLevelBinders tp = case tp of
    NonRecTopBinding b _ _ -> [b]
    RecTopBinding xs -> let (bs, _, _) = unzip3 xs in bs

typeToStringParens : Type -> String
typeToStringParens type_ = case type_ of
    VarTy v -> typeToString (VarTy v)
    TyConApp con ts -> typeToString (TyConApp con ts)
    _ -> "(" ++ typeToString type_ ++ ")"



typeToString : Type -> String
typeToString type_ = case type_ of
    VarTy (BinderId _ getBinder) -> case getBinder () of
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
    LitTy -> "[LitTy]"
    CoercionTy -> "[CoercionTy]"


