module Main exposing (..)

import Types exposing (..)
import Browser
import Browser.Navigation as Nav
import Url exposing (Url)
import Loading exposing (..)

import Pages.Code.Main as Code

main : Program () Model Msg
main = Browser.application
        { init = init
        , view = Code.view
        , update = Code.update
        , subscriptions = Code.subscriptions
        , onUrlChange = MsgUrlChanged
        , onUrlRequest = MsgLinkClicked
        }

init : () -> Url -> Nav.Key -> (Model, Cmd Msg)
init flags url key = 
    let initModel = { projectMetaLoading = Loading Nothing
                    , moduleLoading = Loading Nothing
                    , selectedTerm = Nothing
                    , hideTypes = False
                    , disambiguateVariables = False
                    }
    in (initModel, Code.init)
