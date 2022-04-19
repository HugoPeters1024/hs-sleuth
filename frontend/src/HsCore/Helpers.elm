module HsCore.Helpers exposing (..)

import Char

import Generated.Types exposing (..)

binderName : Binder -> String
binderName binder = case binder of
    Binder b -> b.binderName
    TyBinder b -> b.binderName

binderType : Binder -> Type
binderType binder = case binder of
    Binder b -> b.binderType
    TyBinder b -> b.binderKind

externalName : ExternalName -> String
externalName en = case en of
    ExternalName n -> n.externalName
    ForeignCall -> "ForeignCall"

binderToInt : Binder -> Int
binderToInt binder = case binder of
    Binder b -> binderIdToInt b.binderId
    TyBinder b -> binderIdToInt b.binderId

binderIdToInt : BinderId -> Int
binderIdToInt (BinderId u) = uniqueToInt u

uniqueToInt : Unique -> Int
uniqueToInt (Unique _ i) = i

externalNameToInt : ExternalName -> Int
externalNameToInt en = case en of
    ExternalName n -> uniqueToInt n.externalUnique
    ForeignCall -> -1

isConstructorName : String -> Bool
isConstructorName name = case String.toList name of
    x::_ -> Char.isUpper x
    _    -> False

isTyBinder : Binder -> Bool
isTyBinder b = case b of
    Binder _ -> False
    TyBinder _ -> True

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

getModuleBinders : Module -> List Binder
getModuleBinders mod = List.concatMap getTopLevelBinders mod.moduleTopBindings

unzip3 : List (a,b,c) -> (List a, List b, List c)
unzip3 xs = case xs of
    [] -> ([],[],[])
    ((a,b,c)::ys) -> let (ass, bs, cs) = unzip3 ys in (a::ass, b::bs, c::cs)

getTopLevelBinders : TopBinding -> List Binder
getTopLevelBinders tp = case tp of
    NonRecTopBinding b _ _ -> [b]
    RecTopBinding xs -> let (bs, _, _) = unzip3 xs in bs


