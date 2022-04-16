module PrettyPrint exposing (..)

import Types exposing (..)
import Generated.HsCore as H
import HsCore.Helpers as H
import State exposing (State)
import State as S

import Char
import Either exposing (Either(..))

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

type alias PPState = List (List (Html Msg))

type alias PP = State PPState ()

void : State s a -> State s ()
void s = State.map (\_ -> ()) s

withState : (s -> State s a) -> State s a
withState f = State.get |> State.andThen f

defaultState : PPState
defaultState = []

prettyPrint : PP -> List (Html Msg)
prettyPrint = List.concat << List.intersperse [text "\n"] << List.reverse << runPP []

runPP : PPState -> PP -> List (List (Html Msg))
runPP initial pp = State.finalState initial pp

ppSepped : String -> List PP -> PP
ppSepped s = ppIntercalate (emitText s)

ppIntercalate : PP -> List PP -> PP
ppIntercalate sep pps = case pps of
    (x :: []) -> x
    (x :: xs) -> ppSeq [x, sep, ppIntercalate sep xs]
    [] -> State.modify identity



indented : PP -> PP
indented pp =
    let whitespace = String.fromList (List.repeat 4 ' ')
        block = runPP [] pp
    in State.modify <| \acc -> List.map (\xs -> (text whitespace)::xs) block ++ acc

newline : PP
newline = State.modify <| \acc -> []::acc

ppLines : List PP -> PP
ppLines = ppIntercalate newline

ppSeq : List PP -> PP
ppSeq pps = case pps of
    (x :: xs) -> x |> State.andThen (\_ -> ppSeq xs)
    _ -> State.modify identity

emit : Html Msg -> PP
emit node = State.modify <| \output -> case output of
        x::xs -> (x++[node])::xs
        []   -> [[node]] 

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
    H.EVarGlobal b -> ppExpr (H.EVarGlobal b)
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
ppBinder b = emit <| a [class "no-style", onClick (MsgSelectTerm (Either.Left b))]
                   [ if H.isConstr b
                     then span [class "k"] [text (H.binderName b)]
                     else text (H.binderName b)
                   ]

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
    H.EType t -> emitText (H.showType t)
    _ -> emitText "[Expr TODO]"

ppCase : H.Expr -> H.Binder -> List H.Alt -> PP
ppCase e b alts = ppSeq [emitKeyword "case ", ppExpr e, emitKeyword " of"]

ppExternalName : H.ExternalName -> PP
ppExternalName name = case name of
    H.ExternalName e -> 
        let classes = case String.toList (e.externalName) of
                (c::_) -> if Char.isUpper c
                          then [class "k"]
                          else []
                _ -> []
        in emit <| a [class "no-style", onClick (MsgSelectTerm (Right name))] [span classes [text e.externalName]]
    H.ForeignCall -> emitText "[ForeignCall]"



