module Main exposing (..)

import Html
import Types exposing (..)
import Browser exposing (Document)
import Browser.Navigation as Nav
import Url exposing (Url)
import Loading exposing (..)

import Time
import Task
import Dict exposing (Dict)

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
            , codeTabs = Dict.fromList
                [ ( 0
                  , { id = 0
                    , moduleLoading = Loading Nothing
                    , selectedTerm = Nothing
                    , hideTypes = False
                    , disambiguateVariables = False
                    }
                  )
                ]
            }

init : () -> (Model, Cmd Msg)
init flags = (initModel, Cmd.batch [Task.perform MsgAdjustTimeZone Time.here, Overview.init, Code.init])


viewCodeTabs model =
    let 
        renderTab tab = 
            Tabs.item
                { name = "Code"
                , content = Code.view model tab
                }
    in List.map renderTab (Dict.values model.codeTabs)

view : Model -> Document Msg
view m = 
    { title = "hs-comprehension"
    , body = [ CDN.stylesheet 
             , Tabs.config MsgPageTab
                 |> Tabs.items
                     (
                     [ Tabs.item
                         { name = "Overview"
                         , content = Overview.view m
                         }
                     , Tabs.item
                         { name = "Test"
                         , content = Html.text "yikes"
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
        Just (ntabs, cmd) -> Debug.log "pnig" ({model | codeTabs = ntabs}, cmd)
        Nothing -> Debug.log "invalid code tab" (model, Cmd.none)

