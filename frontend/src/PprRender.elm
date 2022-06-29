module PprRender exposing (..)

import Types exposing (..)
import HsCore.Helpers exposing (..)

import Dict exposing (Dict)

import Ppr exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Pretty
import Pretty.Renderer

import ContextMenu

type alias PprRenderEnv =
    { codeTabId : TabId
    , selectedVar : Maybe Var
    , renameDict : Dict Int String
    , slug : String
    }

varHighlightClass : PprRenderEnv -> Var -> String
varHighlightClass env o = case env.selectedVar of
    Nothing -> ""
    Just v -> 
        if varToInt v == varToInt o
        then ( if varPhaseId v == varPhaseId o
               then "highlight-exact"  
               else "highlight-approx"
             )
        else ""

renderVar : PprRenderEnv -> String -> Var -> Html Msg
renderVar env content var = 
    let className = if varIsConstructor var then "k" else ""
        ctxMenu = ContextMenu.open MsgCtxMenu (CtxCodeVar env.slug var env.codeTabId)
        varName = case Dict.get (varToInt var) env.renameDict of
            Just x -> x
            Nothing -> content
    in a [class "no-style", onClick (MsgCodeMsg env.codeTabId (CodeMsgSelectVar var))]
         [span [ctxMenu, class className, class (varHighlightClass env var)] [text varName]]

htmlTagged : PprRenderEnv -> Tag -> String -> List (Html Msg) -> List (Html Msg)
htmlTagged env tag content next = 
    let htmlContent = case tag of
            TagVar var -> renderVar env content var
            TagLitString -> span [class "s"] [text content]
            TagLitNumber -> span [class "m"] [text content]
            TagKeyword -> span [class "k"] [text content]
            TagComment -> span [class "c1"] [text content]
            TagOperator -> span [class "o"] [text content]
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

renderHtml : PprRenderEnv -> Pretty.Doc Tag -> Html Msg
renderHtml env = Pretty.Renderer.pretty 80 (htmlRenderer env)
