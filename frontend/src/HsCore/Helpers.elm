module HsCore.Helpers exposing (..)

import Char

import Generated.HsCore exposing (..)

binderName : Binder -> String
binderName binder = case binder of
    Binder b -> b.binderName
    TyBinder b -> b.binderName

externalName : ExternalName -> String
externalName en = case en of
    ExternalName n -> n.externalName
    ForeignCall -> "ForeignCall"

binderToInt : Binder -> Int
binderToInt binder = case binder of
    Binder b -> uniqueToInt b.binderId
    TyBinder b -> uniqueToInt b.binderId

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

getTopLevelBinders : Module -> List Binder
getTopLevelBinders mod = List.concatMap getTopLevelBinder mod.moduleTopBindings

getTopLevelBinder : TopBinding -> List Binder
getTopLevelBinder tp =
    let go : TopBinder -> Binder
        go (TopBinder b _ _) = b
    in case tp of
        NonRecTopBinding b -> [go b]
        RecTopBinding bs -> List.map go bs

