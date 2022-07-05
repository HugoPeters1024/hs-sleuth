module Pages.Code exposing (..)

import ElmHelpers as EH

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import HtmlHelpers exposing (..)
import Dict exposing (Dict)
import Generated.Types exposing (..)
import Types exposing (..)
import HsCore.Helpers exposing (..)
import HsCore.Trafo.EraseTypes exposing (eraseTypesPhase)
import HsCore.Trafo.Diff as Diff

import Ppr
import PprRender as Ppr

import Commands as C
import Loading exposing (Loading(..))
import Commands

import Set exposing (Set)

import UI.Slider as Slider

import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Dropdown  as Dropdown
import Bootstrap.Button  as Button
import Bootstrap.Modal as Modal
import Bootstrap.Form.Input as Input

mkCodeMsg : TabId -> CodeTabMsg -> Msg
mkCodeMsg id msg = MsgCodeMsg id msg

subscriptions : CodeTab -> Sub Msg
subscriptions tab = Sub.map (MsgCodeMsg tab.id) (Dropdown.subscriptions tab.moduleDropdown CodeMsgModuleDropdown)

initCodeTabCapture : Int -> Capture -> CodeTabCapture
initCodeTabCapture slot capture = 
    { mod = Loading Nothing
    , capture = capture
    , phaseSlider = Slider.init 0
    , slot = slot
    }

getCaptures : CodeTab -> List Capture
getCaptures tab = List.map .capture (Dict.values tab.captureSlots)

getMergedModuleNames : CodeTab -> List String
getMergedModuleNames tab = List.map Tuple.first (List.concatMap .captureModules (getCaptures tab))
    |> EH.removeDuplicates



makeCodeTab : Model -> List Capture -> (Model, CodeTab, Cmd Msg)
makeCodeTab model captures = 
    let tabId = model.idGen
        tab =
          { id = tabId
          , name = "Code-" ++ String.fromInt tabId
          , captureSlots = Dict.fromList (List.map (\(i, c) -> (i, initCodeTabCapture i c)) (EH.enumerate captures))
          , currentModule = "Main"
          , selectedVar = Nothing
          , moduleDropdown = Dropdown.initialState
          , hideTypes = False
          , disambiguateVariables = False
          , showRecursiveGroups = False
          , selectedTopLevels = []
          , renameModal = 
              { visiblity = Modal.hidden
              , stagingText = ""
              , varId = -1
              }
          , varRenames = Dict.empty
          , toplevelHides = Set.empty
          }
    in
    ( { model | idGen = model.idGen + 1 }
      , tab
      , Cmd.batch (List.map (\ct -> C.fetchModule tabId ct.slot ct.capture.captureName "Main") (Dict.values tab.captureSlots))
    )


update : CodeTabMsg -> CodeTab -> (CodeTab, Cmd Msg)
update msg tab = case msg of
    CodeMsgSetModule modname -> 
        let resetCapture : CodeTabCapture -> CodeTabCapture
            resetCapture x = { x | phaseSlider = Slider.init 0, mod = Loading Nothing }
        in
        ( {tab | currentModule = modname, captureSlots = Dict.map (\_ -> resetCapture) tab.captureSlots }
        , Cmd.batch 
            ( List.map
             (\ct -> Commands.fetchModule tab.id ct.slot ct.capture.captureName modname)
             (Dict.values tab.captureSlots)
            )
        )
    CodeMsgGotModule slot res -> 
        let updateCaptureTab : CodeTabCapture -> CodeTabCapture
            updateCaptureTab tabmod = {tabmod | mod = Loading.loadFromResult res }
        in ({tab | captureSlots = Dict.update slot (Maybe.map updateCaptureTab) tab.captureSlots}, Cmd.none)
    CodeMsgSetPhase slot phase -> 
        let setSlider : CodeTabCapture -> CodeTabCapture
            setSlider tabmod = { tabmod | phaseSlider = Slider.init phase }
        in ({ tab | captureSlots = Dict.update slot (Maybe.map setSlider) tab.captureSlots }, Cmd.none)
    CodeMsgSelectVar var -> ({tab | selectedVar = Just var}, Cmd.none)
    CodeMsgToggleHideTypes -> ({tab | hideTypes = not tab.hideTypes}, Cmd.none)
    CodeMsgToggleDisambiguateVariables -> ({tab | disambiguateVariables = not tab.disambiguateVariables}, Cmd.none)
    CodeMsgToggleShowRecursiveGroups -> ({tab | showRecursiveGroups = not tab.showRecursiveGroups}, Cmd.none)
    CodeMsgModuleDropdown state -> ({tab | moduleDropdown = state}, Cmd.none)
    CodeMsgSlider slot slidermsg ->
        let updateCaptureTab : CodeTabCapture -> CodeTabCapture
            updateCaptureTab tabmod = {tabmod | phaseSlider = Slider.update slidermsg tabmod.phaseSlider }

        in ({tab | captureSlots = Dict.update slot (Maybe.map updateCaptureTab) tab.captureSlots}, Cmd.none)
    CodeMsgMarkTopLevel ti -> ({tab | selectedTopLevels = ti::tab.selectedTopLevels }, Cmd.none)
    CodeMsgRenameModalOpen var -> ({tab | renameModal = renameModalOpen var tab.renameModal}, Cmd.none)
    CodeMsgRenameModalClose -> ({tab | renameModal = renameModalClose tab.renameModal}, Cmd.none)
    CodeMsgRenameModalStagingText txt -> ({tab | renameModal = renameModalSetStaginText txt tab.renameModal}, Cmd.none)
    CodeMsgRenameModalCommit -> 
        let varRenames = Dict.insert tab.renameModal.varId tab.renameModal.stagingText tab.varRenames
            modal = renameModalClose tab.renameModal
        in ({tab | varRenames = varRenames, renameModal = modal}, Cmd.none)

view : Model -> CodeTab -> Html Msg
view model tab = 
    div [] [ viewHeader model tab
           , panel
                (
                    (foreach (Dict.values tab.captureSlots) <| \mod ->
                        (Fraction 4, viewCode model tab mod)
                    )
                    ++
                    [ (Pixels 500, Html.map (mkCodeMsg tab.id) (viewInfo model tab)) ]
                )
           , Html.map (mkCodeMsg tab.id) <| viewRenameModal tab
           ]


selectedTermId : CodeTab -> Maybe Int
selectedTermId tab = Maybe.map varToInt tab.selectedVar


viewHeader : Model -> CodeTab -> Html Msg
viewHeader _ tab = 
    div []
        [ h3 []  [text tab.currentModule]
        , Dropdown.dropdown tab.moduleDropdown
            { options = []
            , toggleMsg = \s -> mkCodeMsg tab.id (CodeMsgModuleDropdown s)
            , toggleButton = Dropdown.toggle [Button.primary] [text "Module"]
            , items = List.map 
                (\modname -> Dropdown.buttonItem [onClick (mkCodeMsg tab.id (CodeMsgSetModule modname))] [text modname]) 
                (getMergedModuleNames tab)
            }
        ]

viewRenameModal : CodeTab -> Html CodeTabMsg
viewRenameModal tab = 
    let msgOnClose = CodeMsgRenameModalClose
        msgOnInput = CodeMsgRenameModalStagingText

    in Modal.config msgOnClose
        |> Modal.small
        |> Modal.hideOnBackdropClick True
        |> Modal.h3 [] [text "Rename Variable"]
        |> Modal.body [] 
            [ Input.text 
                [ Input.value tab.renameModal.stagingText
                , Input.attrs [onInput msgOnInput, autofocus True]
                ]
            ]
        |> Modal.footer []
            [ Button.button
                [ Button.outlinePrimary
                , Button.outlineSuccess
                , Button.attrs [onClick CodeMsgRenameModalCommit]
                ] [text "Apply"]
            , Button.button
                [ Button.outlinePrimary
                , Button.attrs [onClick msgOnClose]
                ] [text "Close"]
            ]
        |> Modal.view tab.renameModal.visiblity


renderVarName : CodeTab -> Var -> String
renderVarName tab var = 
    let postfix : String
        postfix = case var of
            VarBinder b -> 
                let debruijn = (binderId b).binderIdDeBruijn
                in if debruijn == -1 then "" else "_" ++ String.fromInt debruijn
            _ -> ""
    in case Dict.get (varToInt var) tab.varRenames of
    Just name -> name
    Nothing -> (varName tab.disambiguateVariables var) ++ postfix

hideToplevels : Set Int -> Phase -> Phase
hideToplevels hidden phase =
    let q : TopBindingInfo -> Bool
        q ti = Set.member (topBindingInfoToInt ti) hidden

        go : TopBinding -> Maybe TopBinding
        go tb = case tb of
            NonRecTopBinding ti -> if q ti then Nothing else Just (NonRecTopBinding ti)
            RecTopBinding tis -> Just (RecTopBinding (List.filter (not << q) tis))

    in {phase | phaseTopBindings = List.filterMap go phase.phaseTopBindings}

viewCode : Model -> CodeTab -> CodeTabCapture -> Html Msg
viewCode model tab modtab = 
    let pprEnv : Ppr.PprRenderEnv
        pprEnv = 
            { codeTabId = tab.id
            , codeTabSlotId = modtab.slot
            , selectedVar = tab.selectedVar
            , renameDict = tab.varRenames
            , slug = modtab.capture.captureName
            }
    in div []
        [ h4 [] [text modtab.capture.captureName]
        , Loading.renderLoading modtab.mod <| \mod -> 
            case EH.indexList modtab.phaseSlider.value mod.modulePhases of
                Nothing -> text "Invalid Phase Index"
                Just phase -> div []
                    [ text phase.phaseName
                    , Slider.config
                        { lift = \msg -> mkCodeMsg tab.id (CodeMsgSlider modtab.slot msg)
                        , mininum = 0
                        , maximum = List.length mod.modulePhases - 1
                        }
                        |> Slider.view modtab.phaseSlider
                    , pre [class "dark"] 
                        [ code [] 
                               [ processDiff tab phase
                                 |> hideToplevels tab.toplevelHides
                                 |> (if tab.hideTypes then eraseTypesPhase else identity)
                                 |> (if tab.showRecursiveGroups then identity else \p -> {p | phaseTopBindings = removeRecursiveGroups p.phaseTopBindings})
                                 |> Ppr.pprPhase mod.moduleName
                                 |> Ppr.renderHtml pprEnv
                               ]
                        ]
                    ]
        ]


processDiff : CodeTab -> Phase -> Phase
processDiff tab phase = case tab.selectedTopLevels of
    [lhs, rhs] -> 
        let go : TopBindingInfo -> TopBindingInfo
            go tb = 
                if topBindingInfoToInt tb == topBindingInfoToInt lhs && binderPhaseId tb.topBindingBinder == binderPhaseId lhs.topBindingBinder
                then Tuple.first (Diff.anotateTopBindingInfo (lhs, rhs)) 
                else (
                    if topBindingInfoToInt tb == topBindingInfoToInt rhs && binderPhaseId tb.topBindingBinder == binderPhaseId rhs.topBindingBinder
                    then Tuple.second (Diff.anotateTopBindingInfo (lhs, rhs))
                    else tb
                )
                    
        in { phase | phaseTopBindings = List.map (topBindingMap go) phase.phaseTopBindings }
    _          -> phase

fromMaybe : a -> Maybe a -> a
fromMaybe def m = case m of
    Just x -> x
    Nothing -> def

viewInfo : Model -> CodeTab -> Html CodeTabMsg
viewInfo model tab = 
    Card.config []
    |> Card.headerH3 [] [text "Options"]
    |> Card.block []
        [ Block.titleH4 [] [text "View Options"]
        , Block.custom <|
            HtmlHelpers.list 
              [ checkbox tab.hideTypes CodeMsgToggleHideTypes "Hide Types"
              , checkbox tab.disambiguateVariables CodeMsgToggleDisambiguateVariables "Disambiguate Variables Names"
              , checkbox tab.showRecursiveGroups   CodeMsgToggleShowRecursiveGroups "Show Recursive Groups"
              , hr [] []
              , h4 [] [text "Selected Variable"]
              , fromMaybe (h5 [] [text "No term selected"]) (Maybe.map (viewVarInfo tab) tab.selectedVar)
              , hr [] []
              , h4 [] [text "Toplevel functions"]
              ]
        ]
    |> Card.view

viewVarInfo : CodeTab -> Var -> Html CodeTabMsg
viewVarInfo tab term = case term of
    VarBinder b -> viewBinderInfo b
    VarTop tb -> viewTopInfo tab tb
    VarExternal e -> viewExternalInfo e


viewBinderInfo : Binder -> Html CodeTabMsg
viewBinderInfo bndr = case bndr of
    Binder b -> HtmlHelpers.list
            [ text ("name: " ++ b.binderName)
            , text ("type: " ++ typeToString b.binderType)
            , text ("first seen in phase: " ++ String.fromInt b.binderCreatedPhaseId)
            , text ("span: " ++ Debug.toString (b.binderIdInfo))
            ]
    TyBinder b -> text "TODO: TyBinder"

viewTopInfo : CodeTab -> TopBindingInfo -> Html CodeTabMsg
viewTopInfo tab ti = div []
    [ button [onClick (CodeMsgMarkTopLevel ti)] [text "Mark"]
    , text ("#markers: " ++ String.fromInt (List.length tab.selectedTopLevels))
    , viewBinderInfo ti.topBindingBinder
    ]

viewExternalInfo : ExternalName -> Html CodeTabMsg
viewExternalInfo ext = case ext of
    ForeignCall -> text ("[ForeignCall]")
    ExternalName e -> HtmlHelpers.list
        [ text ("name: " ++ e.externalName)
        , text ("module:"  ++ e.externalModuleName)
        , text ("type: " ++ typeToString e.externalType)
        ]




