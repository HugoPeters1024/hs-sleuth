module Types exposing (..)

import Http
import Loading exposing (Loading(..))

import Generated.Types exposing (..)

import UI.Tabs as Tabs
import UI.Slider as Slider
import Time
import Dict exposing (Dict)
import Set exposing (Set)
import Set.Any exposing (AnySet)

import HsCore.Helpers exposing (..) 

import ContextMenu exposing (ContextMenu)

import Bootstrap.Dropdown  as Dropdown
import Bootstrap.Modal as Modal


type alias PhaseId = Int
type alias TabId = Int
type alias VarId = Int
type alias Slug = String
type alias ModuleName = String

type alias CodeTabModule =
    { mod : Loading Module
    , projectMeta : Capture
    , phaseSlider : Slider.Model
    , topNames : List TopBindingInfo
    }

type alias CodeTabRenameModal =
    { visiblity : Modal.Visibility
    , stagingText : String
    , varId : VarId
    }

renameModalSetVis : Modal.Visibility -> CodeTabRenameModal -> CodeTabRenameModal
renameModalSetVis v modal = { modal | visiblity = v }

renameModalSetVarId : VarId -> CodeTabRenameModal -> CodeTabRenameModal
renameModalSetVarId vid modal = { modal | varId = vid }

renameModalClose : CodeTabRenameModal -> CodeTabRenameModal
renameModalClose = renameModalSetVis Modal.hidden

renameModalOpen : Var -> CodeTabRenameModal -> CodeTabRenameModal
renameModalOpen var
    =  renameModalSetVis Modal.shown 
    << renameModalSetVarId (varToInt var)
    << renameModalSetStaginText (varName False var)

renameModalSetStaginText : String -> CodeTabRenameModal -> CodeTabRenameModal
renameModalSetStaginText t modal = { modal | stagingText = t}

type alias CodeTab = 
    { id : TabId
    , name : String
    , modules : Dict Slug CodeTabModule
    , currentModule : ModuleName
    , moduleDropdown : Dropdown.State
    , selectedVar : Maybe Var
    , hideTypes : Bool
    , disambiguateVariables : Bool
    , showRecursiveGroups : Bool 
    , selectedTopLevels : List TopBindingInfo
    , renameModal : CodeTabRenameModal
    , varRenames : Dict Int String
    }

type CodeTabMsg
    = CodeMsgSetModule ModuleName Int
    | CodeMsgGotModule Slug (Result Http.Error Module)
    | CodeMsgSelectVar Var
    | CodeMsgToggleHideTypes
    | CodeMsgToggleDisambiguateVariables
    | CodeMsgToggleShowRecursiveGroups
    | CodeMsgModuleDropdown Dropdown.State
    | CodeMsgSlider Slug Slider.Msg
    | CodeMsgMarkTopLevel TopBindingInfo
    | CodeMsgRenameModalOpen Var
    | CodeMsgRenameModalClose
    | CodeMsgRenameModalStagingText String
    | CodeMsgRenameModalCommit

type CtxMenu =
    CtxCodeVar Var TabId

type alias Model = 
    { pageTab : Tabs.Model
    , capturesLoading : Loading (List Capture)
    , timezone : Time.Zone
    , codeTabs : Dict TabId CodeTab
    , overviewTab : OverviewTab
    , idGen : Int
    , ctxMenu : ContextMenu CtxMenu
    }

type alias OverviewTab =
    { enabledProjects : AnySet Slug Capture
    }

type OverviewMsg
    = OverviewMsgToggleProject Capture


type Msg 
    = MsgGotCaptures (Result Http.Error (List Capture))
    | MsgCodeMsg TabId CodeTabMsg
    | MsgPageTab Tabs.Msg
    | MsgOverViewTab OverviewMsg
    | MsgOpenCodeTab
    | MsgAdjustTimeZone Time.Zone
    | MsgCtxMenu (ContextMenu.Msg CtxMenu)
