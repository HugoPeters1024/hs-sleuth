module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Types exposing (..)
import Browser exposing (Document)
import Url.Builder as Url
import Loading exposing (..)

import Json.Decode exposing (decodeString)

import Generated.Types exposing (..)
import HsCore.Trafo.VarOccs exposing (exprVarOccs)

import Time
import Task
import Dict exposing (Dict)
import Set exposing (Set)

import Pages.Code as Code
import Pages.Overview as Overview
import Ports

import Bootstrap.CDN as CDN
import Bootstrap.Alert as Alert
import Bootstrap.Tab as Tab

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
           { pageTab = Tab.initialState
           , timezone = Time.utc
           , overviewTab = Overview.init
           , idGen = 0
           , codeTabs = Dict.empty
           , ctxMenu = ctxMenu
           }
        , Cmd.batch 
           [ Task.perform MsgAdjustTimeZone Time.here
           , Cmd.map MsgCtxMenu ctxCmd
           ]
        )

addCodeTab : CodeTab -> Model -> Model
addCodeTab tab model = { model | codeTabs = Dict.insert tab.id tab model.codeTabs }

setCodeTabOpen : CodeTab -> Model -> Model
setCodeTabOpen tab model = { model | pageTab = Tab.customInitialState tab.name }

subscriptions : Model -> Sub Msg
subscriptions model = 
    Sub.batch 
        (
            (Sub.map MsgCtxMenu (ContextMenu.subscriptions model.ctxMenu))
            :: (List.map Code.subscriptions (Dict.values model.codeTabs))
        )


viewCodeTabs model =
    let 
        renderTab tab = 
            Tab.item
                { id = tab.name
                , link = Tab.link [] [text tab.name]
                , pane = Tab.pane [] [Code.view model tab]
                }
    in List.map renderTab (Dict.values model.codeTabs)

varToHoogleQuery : Var -> String
varToHoogleQuery var = 
  let goodtype = String.replace "." " ." (typeToString (varType var))
  in Url.crossOrigin "https://hoogle.haskell.org" [] [Url.string "hoogle" (varName var ++ " :: " ++ goodtype)]

getCtxMenuItems : CtxMenu -> List (List (ContextMenu.Item, Msg))
getCtxMenuItems context = case context of
    CtxCodeVar tabid slot var -> 
        let always = 
              [ (ContextMenu.item "Rename", Code.mkCodeMsg tabid (CodeMsgRenameModalOpen var))
              , (ContextMenu.item "Toggle Highlight", Code.mkCodeMsg tabid (CodeMsgHighlightVar var))
              , (ContextMenu.item "Query on Hoogle", MsgOpenBrowserTab (varToHoogleQuery var))
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
        , fontFamily = "inherit"
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
             , Tab.config MsgPageTab
                 |> Tab.items
                     (
                     [ Tab.item
                         { id = "Overview"
                         , link = Tab.link [] [text "Overview"]
                         , pane = Tab.pane [] [Overview.view m]
                         }
                     ]
                     ++ viewCodeTabs m
                     )
                 |> Tab.view m.pageTab
             ] 
    }

updateDictWithEffect : (v -> (v, e)) -> Dict comparable v -> comparable -> Maybe (Dict comparable v, e)
updateDictWithEffect f dict key = Dict.get key dict
   |> Maybe.map (\v -> let (nv, e) = f v in (Dict.insert key nv dict, e))



update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    MsgPageTab pageTab -> ({model | pageTab = pageTab}, Cmd.none)
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
            in (setCodeTabOpen codeTab (addCodeTab codeTab nmodel), cmd)
    MsgCtxMenu ctx -> 
        let (ctxMenu, cmd) = ContextMenu.update ctx model.ctxMenu
        in ({model | ctxMenu = ctxMenu}, Cmd.map MsgCtxMenu cmd)
    MsgOpenBrowserTab url -> (model, Ports.openBrowserTab url)

