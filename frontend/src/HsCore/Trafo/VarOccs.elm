module HsCore.Trafo.VarOccs exposing (exprVarOccs)

import Generated.Types exposing (..)
import HsCore.Helpers exposing (..)
import ElmHelpers as EH

import Set exposing (Set)


exprVarOccs : Expr -> Set Int
exprVarOccs expr = case expr of
  EVar v -> Set.singleton (binderIdToInt v)
  EVarGlobal _ -> Set.empty
  ELit _ -> Set.empty
  EApp f a -> Set.union (exprVarOccs f) (exprVarOccs a)
  ETyLam b e -> Set.insert (binderToInt b) (exprVarOccs e)
  ELam b e -> Set.insert (binderToInt b) (exprVarOccs e)
  ELet bses rhs -> 
    let (bs, es) = List.unzip bses
    in 
      EH.setInsertMany (List.map binderToInt bs) (EH.setCombine (List.map exprVarOccs es))
      |> Set.union (exprVarOccs rhs)
  ECase e b alts -> 
    exprVarOccs e
    |> Set.insert (binderToInt b)
    |> Set.union (EH.setCombine (List.map altVarOccs alts))
  ETick _ e -> exprVarOccs e
  EType _ -> Set.empty
  ECoercion -> Set.empty
  EMarkDiff e -> exprVarOccs e

altVarOccs : Alt -> Set Int
altVarOccs alt = 
  EH.setInsertMany (List.map binderToInt alt.altBinders) (exprVarOccs alt.altRHS)
