module Types exposing (..)

import Http
import Loading exposing (Loading(..))

import Generated.Types exposing (..)
import HsCore.Helpers as H

import UI.Tabs as Tabs
import Time
import Dict exposing (Dict)


type SelectedTerm = SelectedBinder Binder
                  | SelectedTopLevel (Binder, CoreStats)
                  | SelectedExternal ExternalName

selectedTermToInt : SelectedTerm -> Int
selectedTermToInt term = case term of
    SelectedBinder b -> H.binderToInt b
    SelectedTopLevel (b, _) -> H.binderToInt b
    SelectedExternal e -> H.externalNameToInt e

type alias TabId = Int

type alias CodeTab = 
        { id : TabId
        , moduleLoading : Loading Module
        , selectedTerm : Maybe SelectedTerm
        , hideTypes : Bool
        , disambiguateVariables : Bool
        }

type alias Model = 
    { pageTab : Tabs.Model
    , sessionMetaLoading : Loading SessionMeta
    , projectMetaLoading : Loading ProjectMeta
    , timezone : Time.Zone
    , codeTabs : Dict TabId CodeTab
    }

type CodeTabMsg
    = CodeMsgLoadModule String Int
    | CodeMsgGotModule (Result Http.Error Module)
    | CodeMsgSelectTerm SelectedTerm
    | CodeMsgNextPhase Module
    | CodeMsgPrevPhase Module
    | CodeMsgToggleHideTypes
    | CodeMsgToggleDisambiguateVariables


type Msg 
    = MsgGotSessionMeta (Result Http.Error SessionMeta)
    | MsgGotProjectMeta (Result Http.Error ProjectMeta)
    | MsgCodeMsg TabId CodeTabMsg
    | MsgPageTab Tabs.Msg
    | MsgAdjustTimeZone Time.Zone
