module HsCore.Helpers exposing (..)

import Char

import Generated.HsCore exposing (..)

binderName : Binder -> String
binderName binder = case binder of
    Binder b -> b.binderName
    TyBinder b -> b.binderName

isTyBinder : Binder -> Bool
isTyBinder b = case b of
    Binder _ -> False
    TyBinder _ -> True

showType : Type -> String
showType type_ = case type_ of
    VarTy b -> binderName b
    AppTy e a -> showType e ++ " " ++ showType a
    ForAllTy b t -> "forall " ++ binderName b ++ ". " ++ showType t
    _ -> "[TODO Type]"

isConstr : Binder -> Bool
isConstr b = 
    if isTyBinder b
    then False
    else case String.toList (binderName b) of
        (c::_) -> Char.isUpper c
        _      -> False

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
