module PprRender exposing (..)

import Types exposing (..)
import HsCore.Helpers exposing (..)

import Dict exposing (Dict)
import ElmHelpers as EH

import Ppr exposing (..)
import Html exposing (..)
import Html.Lazy
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Pretty
import Pretty.Renderer

import Css
import Css.Global
import Html.Styled

import ContextMenu
import Generated.Types exposing (..)

type alias PprRenderEnv =
    { codeTabId : TabId
    , codeTabSlotId : SlotId
    }

varHighlightClass : PprRenderEnv -> Var -> String
varHighlightClass env o = "var" ++ String.fromInt (varToInt o)

renderVar : PprRenderEnv -> String -> Var -> Html Msg
renderVar env content var = 
    let className = if varIsConstructor var then "k" else ""
        ctxMenu = ContextMenu.open MsgCtxMenu (CtxCodeVar env.codeTabId env.codeTabSlotId var)
        (prefix, base_name) = case EH.popLast (String.split "." content) of
              Just ([], n) -> ("", n)
              Just (qual, n) -> ((String.join "." qual) ++ ".", n)
              _ -> ("", "")
    in a [class "no-style", onClick (MsgCodeMsg env.codeTabId (CodeMsgSelectVar var))]
         [span 
            [ctxMenu, class className, class (varHighlightClass env var), title (typeToString (varType var))] 
            [span [class "nc"] [text prefix], text base_name]]

htmlTagged : PprRenderEnv -> Tag -> String -> List (Html Msg) -> List (Html Msg)
htmlTagged env tag content next = 
    let htmlContent = case tag of
            TagVar var -> renderVar env content var
            TagLitString -> span [class "s"] [text content]
            TagLitNumber -> span [class "m"] [text content]
            TagKeyword -> span [class "k"] [text content]
            TagComment -> span [class "c1"] [text content]
            TagOperator -> span [class "o"] [text content]
            TagModule -> span [class "nc"] [text content]
    in next ++ [htmlContent]

htmlUntagged :  String -> List (Html Msg) -> List (Html Msg)
htmlUntagged txt xs = xs ++ [text txt]

htmlRenderer : PprRenderEnv -> Pretty.Renderer.Renderer Tag (List (Html Msg)) (Html Msg)
htmlRenderer env =
    { init = []
    , tagged = htmlTagged env
    , untagged = htmlUntagged
    , newline = \xs -> xs ++ [text "\n"]
    , outer = span []
    }

dyn_css : Maybe Var -> Html msg
dyn_css selectedVar = 
  let highlight var= Css.Global.class ("var" ++ String.fromInt (varToInt var)) [Css.backgroundColor (Css.rgb 53 100 30)]
  in Html.Styled.toUnstyled <| Css.Global.global (EH.mapMaybe identity [Maybe.map highlight selectedVar])

renderHtml : TabId -> SlotId -> Pretty.Doc Tag -> Html Msg
renderHtml tabid slotid content = 
  let env = 
        { codeTabId = tabid
        , codeTabSlotId = slotid
        }
  in Pretty.Renderer.pretty 80 (htmlRenderer env) content

