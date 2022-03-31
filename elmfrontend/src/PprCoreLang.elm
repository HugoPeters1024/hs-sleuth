module PprCoreLang exposing (viewCoreBind, coreBindBndrUnique, coreBindBndrName, coreBindBndr, isInfixOperator)

import Core.Generated.Types exposing (..)
import MsgTypes exposing (..)

import Html exposing (Html, text, span, a)
import Html.Attributes exposing (class, href)
import Html.Events exposing (onClick)

import Char
import List
import Set
import Dict exposing (Dict)
import State exposing (State)

type alias PPState = { indent : Int
                     , result : List (Html Msg)
                     , toplevel : Bool
                     }

type alias PP = State PPState ()

runPP : PP -> PPState -> PPState
runPP = State.evalState 

defaultState : PPState
defaultState = { indent = 0, result = [], toplevel = True }

isInfixOperator : String -> Bool
isInfixOperator inp = 
    let symbols = Set.fromList (String.toList "!$%&*+./<=>?@\\^-~#")
    in String.all (\c -> Set.member c symbols) inp

viewCoreBind : CoreBind -> List (Html Msg)
viewCoreBind bind = (runPP (ppCoreBind bind |> State.vndThen newline) defaultState).result

coreBindBndr : CoreBind -> CoreId
coreBindBndr (NonRec id _) = id

coreBindBndrName : CoreBind -> String
coreBindBndrName (NonRec id _) = id.name

coreBindBndrUnique : CoreBind -> String
coreBindBndrUnique (NonRec id _) = id.unique

emit : Html Msg -> PP
emit node = State.stateMap (\s -> { s | result = s.result ++ [node] })

emitText : String -> PP
emitText = emit << text

emitOperator : String -> PP
emitOperator op = emit <| span [class "o"] [text op]

emitKeyword : String -> PP
emitKeyword word = emit <| span [class "kt"] [text word]

ppSequence : List PP -> PP
ppSequence = State.void << State.sequence

ppSequenceLines : List PP -> PP
ppSequenceLines pps = State.void (State.sequence (List.map (\pp -> State.vndThen newline pp) pps))

ppa : List (Html Msg) -> Msg -> Html Msg
ppa nodes msg = a [class "no-style", onClick msg] nodes

newline : PP
newline = State.get 
    |> State.andThen (\s -> emit (text ("\n" ++ String.fromList (List.repeat s.indent ' '))))

indented : PP -> PP
indented pp = State.stateMap (\s -> {s | indent = s.indent + 4})
           |> State.vndThen newline
           |> State.vndThen pp
           |> State.vndThen newline
           |> State.vndThen (State.stateMap (\s -> {s | indent = s.indent - 4}))

parens : PP -> PP
parens pp = ppSequence [emitText "(", pp, emitText ")"]

parensTerm : CoreTerm -> PP
parensTerm term = case term of
    Var i -> ppCoreTerm (Var i)
    Lit l -> ppCoreTerm (Lit l)
    t -> parens (ppCoreTerm t)


ppCoreLit : CoreLiteral -> PP
ppCoreLit lit = case lit of
    CoreLitNumber l -> emit <| span [class "mi"] [text l]
    CoreLitString l -> emit <| span [class "s"] [text l]
    CoreLitOther l -> emitText l


ppCoreBndr : CoreId -> PP
ppCoreBndr bndr = State.withState <| \s -> 
    let node = if s.toplevel 
               then span [class "nf"] [text bndr.name] 
               else text bndr.name
    in emit <| ppa [node] (MsgSelectTerm bndr)

ppCoreVar : CoreId -> PP
ppCoreVar id =
    let constructor = case List.head (String.toList id.name) of
            Just x -> Char.isUpper x
            Nothing -> False
        node = if constructor then span [class "kt"] [text id.name] else text id.name
    in emit <| ppa [node] (MsgSelectTerm id)

ppCoreBind : CoreBind -> PP
ppCoreBind (NonRec b e) = ppCoreBndr b 
                                  |> State.vndThen (State.stateMap (\s -> { s | toplevel = False }))
                                  |> State.vndThen (emit (text " = "))
                                  |> State.vndThen (ppCoreTerm e)

ppCoreAltCon : CoreAltCon -> PP
ppCoreAltCon alt = case alt of
    DataAlt i -> emitKeyword i
    LitAlt l -> ppCoreLit l
    DEFAULT -> emitText "_"

ppCoreAlt : CoreAlt -> PP
ppCoreAlt alt = case alt of
    (Alt con bs e) -> ppSequence [ ppCoreAltCon con
                                 , emitText " "
                                 , ppSequence (List.map ppCoreBndr bs)
                                 , emitText " -> "
                                 , ppCoreTerm e
                                 ]


ppCoreTerm : CoreTerm -> PP
ppCoreTerm term = case term of
    Var i -> ppCoreVar i
    Lit l -> ppCoreLit l
    App e a -> 
        let isInfix = case e of
                Var id -> isInfixOperator id.name
                _     -> False

            render = case a of
                Type t -> ppSequence [ppCoreTerm e, emitText " ", emitOperator "@", emitText t]
                _      -> ppSequence [ppCoreTerm e, emitText " ", parensTerm a]

        in render
    Lam b e -> ppSequence [emitText "\\", ppCoreBndr b, emitText " -> ", indented (ppCoreTerm e)]
    Let b e -> ppSequence [ emitKeyword "let "
                          , ppCoreBind b
                          , emitKeyword " in "
                          , indented (ppCoreTerm e)
                          ]
    Case e alts -> ppSequence [ emitKeyword "case "
                              , ppCoreTerm e
                              , emitKeyword " of"
                              , indented <| ppSequenceLines (List.map ppCoreAlt alts)
                              ]
    Type t -> emitText t
    _ -> emitText "Unsupported"



