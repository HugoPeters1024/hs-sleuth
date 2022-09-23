module HsComprehension.Hash where

import Data.Hashable

import HsComprehension.Ast

hashExprModAlpha :: Expr -> Int
hashExprModAlpha (EVar bi) = binderIdDeBruijn bi
hashExprModAlpha (EVarGlobal en) = hash (externalModuleName en <> externalName en)
hashExprModAlpha (ELit lit) = hash lit
hashExprModAlpha (EApp f a) = hashExprModAlpha f + hashExprModAlpha a
hashExprModAlpha (ETyLam bi ex) = hashExprModAlpha ex
hashExprModAlpha (ELam bi ex) = hashExprModAlpha ex
hashExprModAlpha (ELet x1 ex) = hashExprModAlpha ex
hashExprModAlpha (ECase ex bi alts) = sum (map (hashExprModAlpha . altRHS) alts)
hashExprModAlpha (ETick ti ex) = hashExprModAlpha ex
hashExprModAlpha (EType ty) = hashType ty
hashExprModAlpha ECoercion = 0
hashExprModAlpha (EMarkDiff ex) = hashExprModAlpha ex

hashType :: Type -> Int
hashType (VarTy bi) = binderIdDeBruijn bi
hashType (FunTy f a) = hashType f + hashType a
hashType (TyConApp tc tys) = sum (map hashType tys)
hashType (AppTy f a) = hashType f + hashType a
hashType (ForAllTy bi e) = hashType e
hashType (LitTy tylit) = hash tylit
hashType CoercionTy = 0

