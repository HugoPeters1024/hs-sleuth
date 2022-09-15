module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Types exposing (..)
import Browser exposing (Document)
import Loading exposing (..)

import Generated.Types exposing (..)
import HsCore.Trafo.VarOccs exposing (exprVarOccs)

import Time
import Task
import Dict exposing (Dict)
import Set exposing (Set)

import Pages.Code as Code
import Pages.Overview as Overview

import Bootstrap.CDN as CDN

import UI.Tabs as Tabs
import ContextMenu

import HsCore.Helpers exposing (..)

main : Program () Model Msg
main = Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

init : () -> (Model, Cmd Msg)
init _ = 
    let (ctxMenu, ctxCmd) = ContextMenu.init
    in (
           { pageTab = Tabs.init
           , settingsLoading = Loading Nothing
           , capturesLoading = Loading Nothing
           , timezone = Time.utc
           , overviewTab =
               { stagedProjects = []
               }
           , idGen = 0
           , codeTabs = Dict.empty
           , ctxMenu = ctxMenu
           }
        , Cmd.batch 
           [ Task.perform MsgAdjustTimeZone Time.here
           , Overview.init
           , Cmd.map MsgCtxMenu ctxCmd
           ]
        )



addCodeTab : CodeTab -> Model -> Model
addCodeTab tab model = { model | codeTabs = Dict.insert tab.id tab model.codeTabs }

subscriptions : Model -> Sub Msg
subscriptions model = 
    Sub.batch 
        (
            (Sub.map MsgCtxMenu (ContextMenu.subscriptions model.ctxMenu))
            ::
            (List.map Code.subscriptions (Dict.values model.codeTabs))
        )


viewCodeTabs model =
    let 
        renderTab tab = 
            Tabs.item
                { name = tab.name
                , content = Code.view model tab
                }
    in List.map renderTab (Dict.values model.codeTabs)

getCtxMenuItems : CtxMenu -> List (List (ContextMenu.Item, Msg))
getCtxMenuItems context = case context of
    CtxCodeVar tabid slot var -> 
        let always = 
              [ (ContextMenu.item "Rename", Code.mkCodeMsg tabid (CodeMsgRenameModalOpen var))
              , (ContextMenu.item "Toggle Highlight", Code.mkCodeMsg tabid (CodeMsgHighlightVar var))
              ]
            onBinder = 
                let wBinder bndr = case bndr of
                        Binder b -> [(ContextMenu.item ("GOTO First Occ. (Pass " ++ String.fromInt b.binderCreatedPhaseId ++ ")"), Code.mkCodeMsg tabid (CodeMsgSetPhase slot b.binderCreatedPhaseId))]
                        _        -> []
                in case var of
                    VarBinder bndr -> wBinder bndr
                    VarTop t -> wBinder t.topBindingBinder
                    _ -> []
            onToplevel = case var of
                VarTop t -> [ (ContextMenu.item "Hide", Code.mkCodeMsg tabid (CodeMsgHideToplevel slot t))
                            , (ContextMenu.item "Hide all but this", Code.mkCodeMsg tabid (CodeMsgHideToplevelAllBut slot t))
                            , (ContextMenu.item ("Unhide usages in function"), Code.mkCodeMsg tabid (CodeMsgUnhideTransitively slot t))
                            ]
                _        -> []

        in [always ++ onBinder ++ onToplevel]

ctxConfig : ContextMenu.Config
ctxConfig = 
  let d = ContextMenu.defaultConfig
      lightGray = "rgb(238, 238, 238)"
      deepBlue = "rgb(62,126,255)"
  in
    { d
        | direction = ContextMenu.RightBottom
        , overflowX = ContextMenu.Mirror
        , overflowY = ContextMenu.Shift
        , containerColor = lightGray
        , hoverColor = deepBlue
        , invertText = True
        , cursor = ContextMenu.Arrow
        , rounded = True
    }

view : Model -> Document Msg
view m = 
    { title = "hs-comprehension"
    , body = [ CDN.stylesheet 
             , node "link" [rel "stylesheet", href "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.3.0/font/bootstrap-icons.css", type_ "text/css"] []
             , node "link" [rel "stylesheet", href "/style.css", type_ "text/css"] []
             , node "link" [rel "stylesheet", href "/pygments.css", type_ "text/css"] []
             , node "script" [attribute "defer" "", src "highlight.min.js", type_ "text/javascript"] []
             , div []
                [ContextMenu.view
                    ctxConfig
                    MsgCtxMenu
                    getCtxMenuItems
                    m.ctxMenu
                ]
             , Tabs.config MsgPageTab
                 |> Tabs.items
                     (
                     [ Tabs.item
                         { name = "Overview"
                         , content = Overview.view m
                         }
                     ]
                     ++ viewCodeTabs m
                     )
                 |> Tabs.view m.pageTab
             ] 
    }

updateDictWithEffect : (v -> (v, e)) -> Dict comparable v -> comparable -> Maybe (Dict comparable v, e)
updateDictWithEffect f dict key = Dict.get key dict
   |> Maybe.map (\v -> let (nv, e) = f v in (Dict.insert key nv dict, e))

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    MsgGotSettings res -> ({model | settingsLoading = Loading.loadFromResult res}, Cmd.none)
    MsgGotCaptures res -> ({model | capturesLoading = Loading.loadFromResult res}, Cmd.none)
    MsgPageTab tabmsg -> ({model | pageTab = Tabs.update tabmsg model.pageTab}, Cmd.none)
    MsgAdjustTimeZone zone -> ({model | timezone = zone}, Cmd.none)
    MsgCodeMsg tid codemsg -> case updateDictWithEffect (Code.update codemsg) model.codeTabs tid of
        Just (ntabs, cmd) ->({model | codeTabs = ntabs}, cmd)
        Nothing -> (model, Cmd.none)
    MsgOverViewTab tabmsg ->
        let (ntab, cmd) = Overview.update tabmsg model.overviewTab
        in ({model | overviewTab = ntab}, cmd)
    MsgOpenCodeTab -> 
        if (List.isEmpty model.overviewTab.stagedProjects)
        then (model, Cmd.none)
        else
            let (nmodel, codeTab, cmd) = Code.makeCodeTab model model.overviewTab.stagedProjects
            in (addCodeTab codeTab nmodel, cmd)
    MsgCtxMenu ctx -> 
        let (ctxMenu, cmd) = ContextMenu.update ctx model.ctxMenu
        in ({model | ctxMenu = ctxMenu}, Cmd.map MsgCtxMenu cmd) 


