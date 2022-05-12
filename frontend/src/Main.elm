module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Types exposing (..)
import Browser exposing (Document)
import Browser.Navigation as Nav
import Url exposing (Url)
import Loading exposing (..)

import Time
import Task
import Dict exposing (Dict)
import Set exposing (Set)

import Commands
import HsCore.Trafo.Reconstruct as TR
import Pages.Code as Code
import Pages.Overview as Overview

import Bootstrap.CDN as CDN

import UI.Tabs as Tabs

main : Program () Model Msg
main = Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }

initModel : Model
initModel = { pageTab = Tabs.init
            , sessionMetaLoading = Loading Nothing
            , projectMetaLoading = Loading Nothing
            , timezone = Time.utc
            , overviewTab =
                { enabledProjects = Set.empty
                }
            , idGen = 0
            , codeTabs = Dict.empty
            }

addCodeTab : CodeTab -> Model -> Model
addCodeTab tab model = { model | codeTabs = Dict.insert tab.id tab model.codeTabs }

init : () -> (Model, Cmd Msg)
init flags = (initModel, Cmd.batch [Task.perform MsgAdjustTimeZone Time.here, Overview.init, Commands.fetchProjectMeta "secret"])


viewCodeTabs model =
    let 
        renderTab tab = 
            Tabs.item
                { name = tab.name
                , content = Code.view model tab
                }
    in List.map renderTab (Dict.values model.codeTabs)

view : Model -> Document Msg
view m = 
    { title = "hs-comprehension"
    , body = [ CDN.stylesheet 
             , node "link" [rel "stylesheet", href "/style.css", type_ "text/css"] []
             , node "link" [rel "stylesheet", href "/pygments.css", type_ "text/css"] []
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
    MsgGotSessionMeta res -> ({model | sessionMetaLoading = Loading.loadFromResult res}, Cmd.none)
    MsgGotProjectMeta res -> ({model | projectMetaLoading = Loading.loadFromResult res}, Cmd.none)
    MsgPageTab tabmsg -> ({model | pageTab = Tabs.update tabmsg model.pageTab}, Cmd.none)
    MsgAdjustTimeZone zone -> ({model | timezone = zone}, Cmd.none)
    MsgCodeMsg tid codemsg -> case updateDictWithEffect (Code.update codemsg) model.codeTabs tid of
        Just (ntabs, cmd) ->({model | codeTabs = ntabs}, cmd)
        Nothing -> (model, Cmd.none)
    MsgOverViewTab tabmsg ->
        let (ntab, cmd) = Overview.update tabmsg model.overviewTab
        in ({model | overviewTab = ntab}, cmd)
    MsgOpenCodeTab -> 
        if (Set.isEmpty model.overviewTab.enabledProjects)
        then (model, Cmd.none)
        else
            let (nmodel, codeTab, cmd) = Code.makeCodeTab model (Set.toList model.overviewTab.enabledProjects)
            in (addCodeTab codeTab nmodel, cmd)

