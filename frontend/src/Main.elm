module Main exposing (..)

import Types exposing (..)
import Browser exposing (Document)
import Browser.Navigation as Nav
import Url exposing (Url)
import Loading exposing (..)

import Time
import Task

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
            , currentPage = Code
            , sessionMetaLoading = Loading Nothing
            , projectMetaLoading = Loading Nothing
            , moduleLoading = Loading Nothing
            , selectedTerm = Nothing
            , hideTypes = False
            , disambiguateVariables = False
            , timezone = Time.utc
            }

init : () -> (Model, Cmd Msg)
init flags = (initModel, Cmd.batch [Task.perform MsgAdjustTimeZone Time.here, Overview.init, Code.init])

view : Model -> Document Msg
view m = 
    let tabs = [ ("Overview", Overview.view m)
               , ("Code", Code.view m)
               ]
    in { title = "hs-comprehension"
       , body = [ CDN.stylesheet 
                , Tabs.config MsgPageTab
                    |> Tabs.items
                        [ Tabs.item
                            { name = "Overview"
                            , content = Overview.view m
                            }
                        , Tabs.item
                            { name = "Code"
                            , content = Code.view m
                            }
                        ]
                    |> Tabs.view m.pageTab
                ] 
       }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    MsgGotSessionMeta res -> ({model | sessionMetaLoading = Loading.loadFromResult res}, Cmd.none)
    MsgGotProjectMeta res -> ({model | projectMetaLoading = Loading.loadFromResult res}, Cmd.none)
    MsgLoadModule mod id -> ({model | moduleLoading = Loading.setLoading model.moduleLoading}, Commands.fetchPhase mod id)
    MsgGotModule res -> ({ model | moduleLoading = Loading.loadFromResult (Result.map TR.reconModule res)}, Cmd.none)
    MsgSelectTerm term -> ({model | selectedTerm = Just term}, Cmd.none)
    MsgNextPhase mod -> (model, Commands.fetchModifyPhase (\x->x + 1) mod)
    MsgPrevPhase mod -> (model, Commands.fetchModifyPhase (\x->x - 1) mod)
    MsgToggleHideTypes -> ({ model | hideTypes = not model.hideTypes }, Cmd.none)
    MsgToggleDisambiguateVariables -> ({ model | disambiguateVariables = not model.disambiguateVariables }, Cmd.none)
    MsgPageTab tabmsg -> ({model | pageTab = Tabs.update tabmsg model.pageTab}, Cmd.none)
    MsgAdjustTimeZone zone -> ({model | timezone = zone}, Cmd.none)

