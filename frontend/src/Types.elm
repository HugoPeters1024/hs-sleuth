module Types exposing (..)

import Http
import Loading exposing (Loading(..))

import Generated.Types exposing (..)
import HsCore.Helpers as H

import UI.Tabs as Tabs
import Time
import Dict exposing (Dict)
import Set exposing (Set)


type SelectedTerm = SelectedBinder Binder
                  | SelectedTopLevel (Binder, CoreStats)
                  | SelectedExternal ExternalName

selectedTermToInt : SelectedTerm -> Int
selectedTermToInt term = case term of
    SelectedBinder b -> H.binderToInt b
    SelectedTopLevel (b, _) -> H.binderToInt b
    SelectedExternal e -> H.externalNameToInt e

type alias PhaseId = Int
type alias TabId = Int
type alias Slug = String
type alias ModuleName = String

type alias CodeTab = 
    { id : TabId
    , name : String
    , modules : Dict Slug (Loading Module)
    , currentModule : ModuleName
    , currentPhaseId : Int
    , selectedTerm : Maybe SelectedTerm
    , hideTypes : Bool
    , disambiguateVariables : Bool
    }

type CodeTabMsg
    = CodeMsgSetModule ModuleName Int
    | CodeMsgGotModule Slug (Result Http.Error Module)
    | CodeMsgSelectTerm SelectedTerm
    | CodeMsgToggleHideTypes
    | CodeMsgToggleDisambiguateVariables

type alias Model = 
    { pageTab : Tabs.Model
    , sessionMetaLoading : Loading SessionMeta
    , projectMetaLoading : Loading ProjectMeta
    , timezone : Time.Zone
    , codeTabs : Dict TabId CodeTab
    , overviewTab : OverviewTab
    , idGen : Int
    }

type alias OverviewTab =
    { enabledProjects : Set Slug
    }

type OverviewMsg
    = OverviewMsgToggleSlug Slug


type Msg 
    = MsgGotSessionMeta (Result Http.Error SessionMeta)
    | MsgGotProjectMeta (Result Http.Error ProjectMeta)
    | MsgCodeMsg TabId CodeTabMsg
    | MsgPageTab Tabs.Msg
    | MsgOverViewTab OverviewMsg
    | MsgOpenCodeTab
    | MsgAdjustTimeZone Time.Zone
