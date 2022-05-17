module Types exposing (..)

import Http
import Loading exposing (Loading(..))

import Generated.Types exposing (..)
import HsCore.Helpers as H

import UI.Tabs as Tabs
import UI.Slider as Slider
import Time
import Dict exposing (Dict)
import Set exposing (Set)
import Set.Any exposing (AnySet)

import Bootstrap.Dropdown  as Dropdown


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

type alias CodeTabModule =
    { mod : Loading Module
    , projectMeta : ProjectMeta
    , phaseSlider : Slider.Model
    }

type alias CodeTab = 
    { id : TabId
    , name : String
    , modules : Dict Slug CodeTabModule
    , moduleNameSet : Set ModuleName
    , currentModule : ModuleName
    , selectedTerm : Maybe SelectedTerm
    , hideTypes : Bool
    , disambiguateVariables : Bool
    , moduleDropdown : Dropdown.State
    }

type CodeTabMsg
    = CodeMsgSetModule ModuleName Int
    | CodeMsgGotModule Slug (Result Http.Error Module)
    | CodeMsgSelectTerm SelectedTerm
    | CodeMsgToggleHideTypes
    | CodeMsgToggleDisambiguateVariables
    | CodeMsgModuleDropdown Dropdown.State
    | CodeMsgSlider Slug Slider.Msg

type alias Model = 
    { pageTab : Tabs.Model
    , sessionMetaLoading : Loading SessionMeta
    , timezone : Time.Zone
    , codeTabs : Dict TabId CodeTab
    , overviewTab : OverviewTab
    , idGen : Int
    }

type alias OverviewTab =
    { enabledProjects : AnySet Slug ProjectMeta
    }

type OverviewMsg
    = OverviewMsgToggleProject ProjectMeta


type Msg 
    = MsgGotSessionMeta (Result Http.Error SessionMeta)
    | MsgCodeMsg TabId CodeTabMsg
    | MsgPageTab Tabs.Msg
    | MsgOverViewTab OverviewMsg
    | MsgOpenCodeTab
    | MsgAdjustTimeZone Time.Zone
