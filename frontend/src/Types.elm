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
import Bootstrap.Alert as Alert
import File exposing (File)
import Zip
import Zip.Entry exposing (Entry)
import Bytes exposing (Bytes)


type alias PhaseId = Int
type alias TabId = Int
type alias SlotId = Int
type alias VarId = Int
type alias Slug = String
type alias ModuleName = String


type SrcToggle = Core | Src
toggleSrc : SrcToggle -> SrcToggle
toggleSrc t = case t of
  Core -> Src
  Src -> Core

type alias CodeTabCapture =
    { mod : Module
    , srcLoading : Loading String
    , capture_view : CaptureView
    , phaseSlider : Slider.Model
    , slot : Int
    , toplevelHides : Set Int
    , srcToggle : SrcToggle
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
    << renameModalSetStagingText (varName var)

renameModalSetStagingText : String -> CodeTabRenameModal -> CodeTabRenameModal
renameModalSetStagingText t modal = { modal | stagingText = t}

type alias CodeViewOptions =
  { hideTypes : Bool
  , hideModules : Bool
  , hideDisambiguation : Bool
  , hideRecursiveGroups : Bool 
  , hideUndemanded : Bool
  , varRenames : Dict Int String
  }

codeViewOptionsToggleHideTypes o = { o | hideTypes = not o.hideTypes }
codeViewOptionsToggleHideModules o = { o | hideModules = not o.hideModules }
codeViewOptionsToggleHideDisambiguation o = { o | hideDisambiguation = not o.hideDisambiguation }
codeViewOptionsToggleHideRecursiveGroups o = { o | hideRecursiveGroups = not o.hideRecursiveGroups }
codeViewOptionsToggleHideUndemanded o = { o | hideUndemanded = not o.hideUndemanded }
codeViewOptionsMapVarRenames f o = { o | varRenames = f o.varRenames }

type alias CaptureView =
    { capture: Capture
    , files: Dict String Entry
    }

type alias CodeTab = 
    { id : TabId
    , name : String
    , captureSlots : Dict Int CodeTabCapture
    , currentModule : ModuleName
    , moduleDropdown : Dropdown.State
    , codeViewOptions : CodeViewOptions
    , selectedVar : Maybe Var
    , selectedTopLevels : List TopBindingInfo
    , renameModal : CodeTabRenameModal
    , varHighlights : Set Int
    }

type CodeTabMsg
    = CodeMsgSetModule ModuleName
    | CodeMsgSetPhase SlotId Int
    | CodeMsgSelectVar Var
    | CodeMsgToggleSrc SlotId
    | CodeMsgToggleHideTypes
    | CodeMsgToggleHideModules
    | CodeMsgToggleHideDisambiguation
    | CodeMsgToggleHideRecursiveGroups
    | CodeMsgToggleHideUndemanded
    | CodeMsgModuleDropdown Dropdown.State
    | CodeMsgSlider SlotId Slider.Msg
    | CodeMsgRenameModalOpen Var
    | CodeMsgRenameModalClose
    | CodeMsgRenameModalStagingText String
    | CodeMsgRenameModalCommit
    | CodeMsgHideToplevel SlotId TopBindingInfo
    | CodeMsgHideToplevelAllBut SlotId TopBindingInfo
    | CodeMsgUnhideTransitively SlotId TopBindingInfo
    | CodeMsgHideToplevelDiffTemplate
    | CodeMsgUnhideAll
    | CodeMsgHighlightVar Var
    | CodeMsgRemoveAllHightlights

type CtxMenu =
    CtxCodeVar TabId SlotId Var

type alias Model = 
    { pageTab : Tabs.Model
    , timezone : Time.Zone
    , codeTabs : Dict TabId CodeTab
    , overviewTab : OverviewTab
    , idGen : Int
    , ctxMenu : ContextMenu CtxMenu
    }

type alias OverviewTab =
    { stagedProjects : List CaptureView
    , problem: Maybe String
    , captures : List CaptureView
    }

overviewSetProblem : String -> OverviewTab -> OverviewTab
overviewSetProblem problem tab = { tab | problem = Just problem }

overviewRemoveProblem : OverviewTab -> OverviewTab
overviewRemoveProblem tab = { tab | problem = Nothing }



type OverviewMsg
    = OverviewMsgStageCapture CaptureView
    | OverviewMsgTriggerFile
    | OverviewMsgGotFile File
    | OverviewMsgReadFile Bytes
    | OverviewMsgDismissProblem Alert.Visibility

type Msg 
    = MsgCodeMsg TabId CodeTabMsg
    | MsgPageTab Tabs.Msg
    | MsgOverViewTab OverviewMsg
    | MsgOpenCodeTab
    | MsgAdjustTimeZone Time.Zone
    | MsgCtxMenu (ContextMenu.Msg CtxMenu)
