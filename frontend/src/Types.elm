module Types exposing (..)

import Http
import Html exposing (Html, text)
import Browser
import Url exposing (Url)

import Either exposing (Either)
import Loading exposing (Loading(..))

import Generated.Types exposing (..)
import HsCore.Helpers as H

import UI.Tabs as Tabs
import Time


type SelectedTerm = SelectedBinder Binder
                  | SelectedTopLevel (Binder, CoreStats)
                  | SelectedExternal ExternalName

selectedTermToInt : SelectedTerm -> Int
selectedTermToInt term = case term of
    SelectedBinder b -> H.binderToInt b
    SelectedTopLevel (b, _) -> H.binderToInt b
    SelectedExternal e -> H.externalNameToInt e

type Page = Overview | Code

type alias Model = 
    { pageTab : Tabs.Model
    , currentPage : Page
    , sessionMetaLoading : Loading SessionMeta
    , projectMetaLoading : Loading ProjectMeta
    , moduleLoading : Loading Module
    , selectedTerm : Maybe SelectedTerm
    , hideTypes : Bool
    , disambiguateVariables : Bool
    , timezone : Time.Zone
    }


type Msg = MsgGotSessionMeta (Result Http.Error SessionMeta)
         | MsgGotProjectMeta (Result Http.Error ProjectMeta)
         | MsgLoadModule String Int
         | MsgGotModule (Result Http.Error Module)
         | MsgSelectTerm SelectedTerm
         | MsgNextPhase Module
         | MsgPrevPhase Module
         | MsgToggleHideTypes
         | MsgToggleDisambiguateVariables
         | MsgPageTab Tabs.Msg
         | MsgAdjustTimeZone Time.Zone
