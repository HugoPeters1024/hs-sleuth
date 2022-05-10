module Main exposing (..)

import Types exposing (..)
import Browser exposing (Document)
import Browser.Navigation as Nav
import Url exposing (Url)
import Loading exposing (..)

import Commands
import HsCore.Trafo.Reconstruct as TR
import Pages.Code as Code
import Pages.Overview as Overview

main : Program () Model Msg
main = Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = Code.subscriptions
        , onUrlChange = MsgUrlChanged
        , onUrlRequest = MsgLinkClicked
        }

initModel : Model
initModel = { currentPage = Overview
            , sessionMetaLoading = Loading Nothing
            , projectMetaLoading = Loading Nothing
            , moduleLoading = Loading Nothing
            , selectedTerm = Nothing
            , hideTypes = False
            , disambiguateVariables = False
            }

init : () -> Url -> Nav.Key -> (Model, Cmd Msg)
init flags url key = (initModel, Overview.init)

view : Model -> Document Msg
view m = 
    let (title, body) = case m.currentPage of
            Overview -> ("Overview", Overview.view m)
            Code     -> ("Code", Code.view m)
    in { title = title, body = [body] }

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
    _       -> (model, Cmd.none)

