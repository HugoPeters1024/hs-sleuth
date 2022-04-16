module PrettyPrint exposing (..)

import Types exposing (..)
import Generated.HsCore as H
import HsCore.Helpers as H
import State exposing (State)
import State as S

import Char

import Html exposing (..)
import Html.Attributes exposing (..)

type alias PPState =
    { output: List (Html Msg)
    , indent : Int
    }

type alias PP = State PPState ()

void : State s a -> State s ()
void s = State.map (\_ -> ()) s

withState : (s -> State s a) -> State s a
withState f = State.get |> State.andThen f

defaultState : PPState
defaultState = { output = [], indent = 0 }

runPP : PPState -> PP -> List (Html Msg)
runPP initial pp = let (_, final) = State.run initial pp in final.output

ppSepped : String -> List PP -> PP
ppSepped s = ppIntercalate (emitText s)

ppIntercalate : PP -> List PP -> PP
ppIntercalate sep pps = case pps of
    (x :: []) -> x
    (x :: xs) -> ppSeq [x, sep, ppIntercalate sep xs]
    [] -> State.modify identity

indented : PP -> PP
indented pp = ppSeq [ State.modify (\s -> { s | indent = s.indent + 4})
                    , newline
                    , pp
                    , State.modify (\s -> {s | indent = s.indent - 4})
                    ]

newline : PP
newline = withState <| \s -> emitText ("\n" ++ String.fromList (List.repeat s.indent ' '))

ppLines : List PP -> PP
ppLines = ppIntercalate newline

ppSeq : List PP -> PP
ppSeq pps = case pps of
    (x :: xs) -> x |> State.andThen (\_ -> ppSeq xs)
    _ -> State.modify identity

emit : Html Msg -> PP
emit node = State.modify <| \s -> {s | output = s.output ++ [node]}

emitText : String -> PP
emitText msg = emit (text msg)

emitSpan : String -> String -> PP
emitSpan c msg = emit (span [class c] [text msg])

emitKeyword : String -> PP
emitKeyword = emitSpan "k"

parens : PP -> PP
parens pp = ppSeq [emitText "(", pp, emitText ")"]

parensExpr : H.Expr -> PP
parensExpr expr = case expr of
    H.EVar b -> ppExpr (H.EVar b)
    H.ELit l -> ppExpr (H.ELit l)
    H.EType t -> ppExpr (H.EType t)
    _        -> parens (ppExpr expr)

ppLit : H.Lit -> PP
ppLit lit  = case lit of
    H.MachChar c -> emitSpan "s" (String.fromList ['\'', c, '\''])
    H.MachStr s  -> emitSpan "s" ("\"" ++ s ++ "\"")
    H.MachInt i  -> emitSpan "m" i
    _            -> emitText (Debug.toString lit)

ppBinder : H.Binder -> PP
ppBinder b = 
    if H.isConstr b
    then emitKeyword (H.binderName b)
    else emitText (H.binderName b)

ppTopBinding : H.TopBinding -> PP
ppTopBinding b = case b of
    H.NonRecTopBinding bndr -> ppTopBinder bndr
    H.RecTopBinding xs -> Debug.todo "Recursive bindings not implemented"


ppTopBinder : H.TopBinder -> PP
ppTopBinder (H.TopBinder b _ e) = ppBinding (b,e)

ppBinding : (H.Binder, H.Expr) -> PP
ppBinding (b, e) = 
    let (fe, bs) = H.leadingLambdas e in ppSeq [ppSepped " " (List.map ppBinder (b::bs)), emitText " = ", ppExpr fe]

ppExpr : H.Expr -> PP
ppExpr expr = case expr of
    H.EVar b -> ppBinder b
    H.EVarGlobal name -> ppExternalName name
    H.ELit lit -> ppLit lit
    H.ETyLam b e -> ppExpr (H.ELam b e)
    H.EApp f a -> ppSeq [ppExpr f, emitText " ", parensExpr a]
    H.ELam b e -> ppSeq [emitText "\\", ppBinder b, emitText " -> ", indented (ppExpr e)]
    H.ELet bs e -> ppSeq [ emitKeyword "let "
                         , indented <| ppLines (List.map ppBinding bs)
                         , newline
                         , emitKeyword " in ", ppExpr e]
    H.ECase e b alts -> ppCase e b alts
    H.EType _ -> emitText "[TODO Type]"
    _ -> emitText "[Expr TODO]"

ppCase : H.Expr -> H.Binder -> List H.Alt -> PP
ppCase e b alts = ppSeq [emitKeyword "case ", ppExpr e, emitKeyword " of"]

ppExternalName : H.ExternalName -> PP
ppExternalName name = case name of
    H.ExternalName e -> case String.toList (e.externalName) of
        (c::_) -> if Char.isUpper c
                  then emitKeyword e.externalName
                  else emitText e.externalName
        _ -> emitText e.externalName
    H.ForeignCall -> emitText "[ForeignCall]"



