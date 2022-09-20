module Pages.Code exposing (..)

import ElmHelpers as EH

import Http
import Html exposing (..)
import Html.Lazy
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import HtmlHelpers exposing (..)
import Dict exposing (Dict)
import Generated.Types exposing (..)
import Types exposing (..)
import HsCore.Helpers exposing (..)
import HsCore.Trafo.EraseTypes exposing (eraseTypesPhase)
import HsCore.Trafo.VarOccs exposing (exprVarOccs)
import HsCore.Trafo.Diff as Diff

import Ppr
import PprRender as Ppr

import Commands as C
import Loading exposing (Loading(..))
import Commands

import Set exposing (Set)

import UI.Slider as Slider

import Bootstrap.Card as Card
import Bootstrap.Spinner as Spinner
import Bootstrap.Card.Block as Block
import Bootstrap.Dropdown  as Dropdown
import Bootstrap.Button  as Button
import Bootstrap.Modal as Modal
import Bootstrap.Form.Input as Input

mkCodeMsg : TabId -> CodeTabMsg -> Msg
mkCodeMsg id msg = MsgCodeMsg id msg

subscriptions : CodeTab -> Sub Msg
subscriptions tab = Sub.batch
  (
  [ Sub.map (MsgCodeMsg tab.id) (Dropdown.subscriptions tab.moduleDropdown CodeMsgModuleDropdown)
  ] ++ 
    (Dict.values tab.captureSlots
     |> List.map (\cap -> Http.track (String.fromInt cap.slot) (mkCodeMsg tab.id << CodeMsgHttpTrack cap.slot))
    )
  )

initCodeTabCapture : Int -> Capture -> CodeTabCapture
initCodeTabCapture slot capture = 
    { mod = Loading Nothing
    , modRequestProgress = 0
    , srcLoading = Loading Nothing
    , capture = capture
    , phaseSlider = Slider.init 0
    , slot = slot
    , toplevelHides = Set.empty
    , srcToggle = Core
    }

getCurrentCaptures : CodeTab -> List Capture
getCurrentCaptures tab = List.map .capture (Dict.values tab.captureSlots)

getCurrentCaptureTabs : CodeTab -> List CodeTabCapture
getCurrentCaptureTabs tab = Dict.values tab.captureSlots

getMergedModuleNames : CodeTab -> List String
getMergedModuleNames tab = List.map Tuple.first (List.concatMap .captureModules (getCurrentCaptures tab))
    |> EH.removeDuplicates

getModules : CodeTab -> List Module
getModules tab = EH.mapMaybe Loading.toMaybe (List.map .mod (getCurrentCaptureTabs tab))

getCurrentPhases : CodeTab -> List Phase
getCurrentPhases tab =
  let go : CodeTabCapture -> Maybe Phase
      go capture = 
        capture.mod 
          |> Loading.toMaybe
          |> Maybe.andThen (\mod -> EH.indexList capture.phaseSlider.value mod.modulePhases)
  in EH.mapMaybe go (Dict.values tab.captureSlots)

getMatchedTopLevel : (TopBindingInfo -> Int) -> CodeTab -> List (List TopBindingInfo)
getMatchedTopLevel lens tab =
  let insert : TopBindingInfo -> Dict Int (List TopBindingInfo) -> Dict Int (List TopBindingInfo)
      insert ti dict =
        let id = lens ti
            up mxs = case mxs of
              Just xs -> Just (ti::xs)
              Nothing -> Just [ti]
        in Dict.update id up dict

      go : List TopBindingInfo -> Dict Int (List TopBindingInfo) -> Dict Int (List TopBindingInfo)
      go xs acc = List.foldl insert acc xs

  in Dict.values <| List.foldl go Dict.empty (List.map getPhaseTopBinders (getCurrentPhases tab))

makeCodeTab : Model -> List Capture -> (Model, CodeTab, Cmd Msg)
makeCodeTab model captures = 
    let tabId = model.idGen
        tab : CodeTab
        tab =
          { id = tabId
          , name = "Code-" ++ String.fromInt tabId
          , captureSlots = Dict.fromList (List.map (\(i, c) -> (i, initCodeTabCapture i c)) (EH.enumerate captures))
          , currentModule = 
              List.head captures
              |> Maybe.andThen (.captureModules >> List.map Tuple.first >> List.head)
              |> Maybe.withDefault "Main"
          , selectedVar = Nothing
          , varHighlights = Set.empty
          , moduleDropdown = Dropdown.initialState
          , codeViewOptions = 
            { hideTypes = False
            , hideModules = False
            , hideDisambiguation = True
            , hideRecursiveGroups = True
            , hideUndemanded = True
            , varRenames = Dict.empty
          }
          , selectedTopLevels = []
          , renameModal = 
              { visiblity = Modal.hidden
              , stagingText = ""
              , varId = -1
              }
          }
    in
    ( { model | idGen = model.idGen + 1 }
      , tab
      , Cmd.batch (List.map (\ct -> C.fetchModuleWithSrc tabId ct.slot ct.capture.captureName tab.currentModule) (Dict.values tab.captureSlots))
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
             (\ct -> Commands.fetchModuleWithSrc tab.id ct.slot ct.capture.captureName modname)
             (Dict.values tab.captureSlots)
            )
        )
    CodeMsgGotModule slot res -> 
        let updateCaptureTab : CodeTabCapture -> CodeTabCapture
            updateCaptureTab tabmod = {tabmod | mod = Loading.loadFromResult res }
        in ({tab | captureSlots = Dict.update slot (Maybe.map updateCaptureTab) tab.captureSlots}, Cmd.none)
    CodeMsgGotSrc slot res -> 
        let updateCaptureTab : CodeTabCapture -> CodeTabCapture
            updateCaptureTab tabmod = {tabmod | srcLoading = Loading.loadFromResult res }
        in ({tab | captureSlots = Dict.update slot (Maybe.map updateCaptureTab) tab.captureSlots}, Cmd.none)
    CodeMsgSetPhase slot phase -> 
        let setSlider : CodeTabCapture -> CodeTabCapture
            setSlider tabmod = { tabmod | phaseSlider = Slider.init phase }
        in ({ tab | captureSlots = Dict.update slot (Maybe.map setSlider) tab.captureSlots }, Cmd.none)
    CodeMsgSelectVar var -> ({tab | selectedVar = Just var}, Cmd.none)
    CodeMsgToggleSrc slot -> 
        let setSlider : CodeTabCapture -> CodeTabCapture
            setSlider tabmod = { tabmod | srcToggle = toggleSrc tabmod.srcToggle }
        in ({ tab | captureSlots = Dict.update slot (Maybe.map setSlider) tab.captureSlots }, Cmd.none)
    CodeMsgToggleHideTypes -> ({tab | codeViewOptions = codeViewOptionsToggleHideTypes tab.codeViewOptions}, Cmd.none)
    CodeMsgToggleHideModules -> ({tab | codeViewOptions = codeViewOptionsToggleHideModules tab.codeViewOptions}, Cmd.none)
    CodeMsgToggleHideDisambiguation -> ({tab | codeViewOptions = codeViewOptionsToggleHideDisambiguation tab.codeViewOptions}, Cmd.none)
    CodeMsgToggleHideRecursiveGroups -> ({tab | codeViewOptions = codeViewOptionsToggleHideRecursiveGroups tab.codeViewOptions}, Cmd.none)
    CodeMsgToggleHideUndemanded -> ({tab | codeViewOptions = codeViewOptionsToggleHideUndemanded tab.codeViewOptions}, Cmd.none)
    CodeMsgModuleDropdown state -> ({tab | moduleDropdown = state}, Cmd.none)
    CodeMsgSlider slot slidermsg ->
        let updateCaptureTab : CodeTabCapture -> CodeTabCapture
            updateCaptureTab tabmod = {tabmod | phaseSlider = Slider.update slidermsg tabmod.phaseSlider }

        in ({tab | captureSlots = Dict.update slot (Maybe.map updateCaptureTab) tab.captureSlots}, Cmd.none)
    CodeMsgRenameModalOpen var -> ({tab | renameModal = renameModalOpen var tab.renameModal}, Cmd.none)
    CodeMsgRenameModalClose -> ({tab | renameModal = renameModalClose tab.renameModal}, Cmd.none)
    CodeMsgRenameModalStagingText txt -> ({tab | renameModal = renameModalSetStagingText txt tab.renameModal}, Cmd.none)
    CodeMsgRenameModalCommit -> 
        let varRenames = Dict.insert tab.renameModal.varId tab.renameModal.stagingText tab.codeViewOptions.varRenames
            modal = renameModalClose tab.renameModal
        in ({tab | codeViewOptions = codeViewOptionsMapVarRenames (\_ -> varRenames) tab.codeViewOptions, renameModal = modal}, Cmd.none)
    CodeMsgHideToplevel slot ti -> 
        let updateHideSet : CodeTabCapture -> CodeTabCapture
            updateHideSet tabmod = {tabmod | toplevelHides = EH.toggleSet (topBindingInfoToInt ti) tabmod.toplevelHides}
        in ({tab | captureSlots = Dict.update slot (Maybe.map updateHideSet) tab.captureSlots}, Cmd.none)
    CodeMsgHideToplevelAllBut slot ti -> 
      let allVarIds =
            getModules tab
            |> List.concatMap .modulePhases
            |> List.concatMap .phaseTopBindings
            |> List.concatMap HsCore.Helpers.getTopLevelBinders
            |> List.map HsCore.Helpers.topBindingInfoToInt
            |> Set.fromList

          updateHideSet : CodeTabCapture -> CodeTabCapture
          updateHideSet tabmod = {tabmod | toplevelHides = EH.toggleSet (topBindingInfoToInt ti) allVarIds}
      in ({tab | captureSlots = Dict.update slot (Maybe.map updateHideSet) tab.captureSlots}, Cmd.none)

    CodeMsgUnhideTransitively slot ti -> 
      let updateHideSet : CodeTabCapture -> CodeTabCapture
          updateHideSet tabmod = {tabmod | toplevelHides = Set.diff tabmod.toplevelHides (exprVarOccs ti.topBindingRHS) }
      in ({tab | captureSlots = Dict.update slot (Maybe.map updateHideSet) tab.captureSlots}, Cmd.none)
        
    CodeMsgHideToplevelDiffTemplate -> 
        let 
            -- Predicate used to determine wether to hide toplevel defs
            -- they must all have the same hash (inferred by the bucket being full)
            -- It can also be that the bucket is fuller because multiple toplevel definitions
            -- are the same, this is a currently unaddressed problem
            pred : List TopBindingInfo -> Bool
            pred xs = List.length xs >= Dict.size tab.captureSlots-- && EH.allSame xs

            hideSet : Set Int
            hideSet = 
              getMatchedTopLevel .topBindingHash tab
              |> List.filter pred
              |> List.concat
              |> List.map topBindingInfoToInt
              |> Set.fromList

            updateHideSet : CodeTabCapture -> CodeTabCapture
            updateHideSet tabmod = {tabmod | toplevelHides = hideSet }
        in ({tab | captureSlots = Dict.map (\_ c -> updateHideSet c) tab.captureSlots}, Cmd.none)
    CodeMsgUnhideAll ->
      let updateHideSet : CodeTabCapture -> CodeTabCapture
          updateHideSet tabmod = {tabmod | toplevelHides = Set.empty}
      in ({tab | captureSlots = Dict.map (\_ -> updateHideSet) tab.captureSlots}, Cmd.none)
    CodeMsgHighlightVar var -> ({tab | varHighlights = EH.toggleSet (HsCore.Helpers.varToInt var) tab.varHighlights}, Cmd.none)
    CodeMsgRemoveAllHightlights -> ({tab | varHighlights = Set.empty}, Cmd.none)
    CodeMsgHttpTrack slot p -> 
      let updateProgress : CodeTabCapture -> CodeTabCapture
          updateProgress c = {c | modRequestProgress = EH.progressBytes p}
      in ({tab | captureSlots = Dict.update slot (Maybe.map updateProgress) tab.captureSlots}, Cmd.none)




view : Model -> CodeTab -> Html Msg
view model = Html.Lazy.lazy <| \tab ->
    div [] [ viewHeader model tab
           , panel
                (
                    (Pixels 500, Html.map (mkCodeMsg tab.id) (viewInfo model tab))
                    ::
                    (foreach (Dict.values tab.captureSlots) <| \mod ->
                        (Fraction 4, viewCode tab mod)
                    )
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


hideToplevels : Set Int -> Phase -> Phase
hideToplevels hidden phase =
    let q : TopBindingInfo -> Bool
        q ti = Set.member (topBindingInfoToInt ti) hidden

        go : TopBinding -> Maybe TopBinding
        go tb = case tb of
            NonRecTopBinding ti -> if q ti then Nothing else Just (NonRecTopBinding ti)
            RecTopBinding tis -> Just (RecTopBinding (List.filter (not << q) tis))

    in {phase | phaseTopBindings = List.filterMap go phase.phaseTopBindings}

renderPhase : CodeViewOptions -> ModuleName -> Set Int -> Int -> Int -> Phase -> Html Msg
renderPhase cv modname toplevelHides tabid panelid phase = 
  phase
  |> hideToplevels toplevelHides
  |> (if cv.hideTypes then eraseTypesPhase else identity)
  |> (if cv.hideRecursiveGroups then \p -> {p | phaseTopBindings = removeRecursiveGroups p.phaseTopBindings} else identity)
  |> Ppr.pprPhase cv modname
  |> Ppr.renderHtml tabid panelid

viewCode : CodeTab -> CodeTabCapture -> Html Msg
viewCode tab modtab = div []
        [ h4 [] [text modtab.capture.captureName]
        , Loading.renderLoadingWith (span [] [Spinner.spinner [] [], text ("downloaded " ++ String.fromInt (modtab.modRequestProgress // (1024 * 1024)) ++ "mb") ]) modtab.mod <| \mod -> 
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
                      [ case modtab.srcToggle of
                          Core -> code [] 
                                       [ a [class "src-toggle", onClick (mkCodeMsg tab.id (CodeMsgToggleSrc modtab.slot))] [text "view source\n\n"]
                                       , Ppr.dyn_css tab.varHighlights tab.selectedVar
                                       , Html.Lazy.lazy6 renderPhase tab.codeViewOptions mod.moduleName modtab.toplevelHides tab.id modtab.slot phase
                                       ]
                                 
                          Src -> code [class "language-haskell"]
                                      [ a [class "src-toggle", onClick (mkCodeMsg tab.id (CodeMsgToggleSrc modtab.slot))] [text "view core\n\n"]
                                      , Loading.renderLoading modtab.srcLoading text
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
              [ checkbox tab.codeViewOptions.hideTypes CodeMsgToggleHideTypes "Hide Types"
              , checkbox tab.codeViewOptions.hideModules CodeMsgToggleHideModules "Hide Module Qualifiers"
              , checkbox tab.codeViewOptions.hideDisambiguation CodeMsgToggleHideDisambiguation "Hide Uniques"
              , checkbox tab.codeViewOptions.hideRecursiveGroups   CodeMsgToggleHideRecursiveGroups "Hide Recursive Grouping"
              , checkbox tab.codeViewOptions.hideUndemanded CodeMsgToggleHideUndemanded "Render Undemanded Variables as _"
              , hr [] []
              , h4 [] [text "Selected Variable"]
              , fromMaybe (h5 [] [text "No term selected"]) (Maybe.map (viewVarInfo tab) tab.selectedVar)
              , hr [] []
              , viewHideOptions model tab
              , hr [] []
              , viewExtraOptions model tab
              ]
        ]
    |> Card.view

viewHideOptions : Model -> CodeTab -> Html CodeTabMsg
viewHideOptions model tab = HtmlHelpers.list
  [ h4 [] [text "Hide Options"]
  , text (String.fromInt (List.sum (List.map (Set.size << .toplevelHides) (getCurrentCaptureTabs tab))) ++ " toplevels currently hidden")
  , Button.button 
      [ Button.info
      , Button.disabled (Dict.size tab.captureSlots < 2)
      , Button.attrs 
        [ onClick CodeMsgHideToplevelDiffTemplate
        , title (if (Dict.size tab.captureSlots < 2) then "At least 2 open captures required" else "Hide toplevel definitions that do not differ")
        ]
      ]
      [text "Hide Common Definitions"]
  , Button.button 
      [ Button.info
      , Button.attrs 
        [ onClick CodeMsgUnhideAll
        ]
      ]
      [text "Unhide all"]
  ]

viewExtraOptions : Model -> CodeTab -> Html CodeTabMsg
viewExtraOptions model tab = HtmlHelpers.list
  [ h4 [] [text "Other Options"]
  , Button.button
      [ Button.info
      , Button.disabled (Set.size tab.varHighlights == 0)
      , Button.attrs
        [ onClick CodeMsgRemoveAllHightlights
        , title (if (Set.size tab.varHighlights == 0) then "No terms currently highlighted" else "Remove all highlights from currently highlighted variables")
        ]
      ]
      [text "Remove All Highlights"]
  ]



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
--            , text ("span: " ++ Debug.toString (b.binderIdInfo))
            ]
    TyBinder b -> text "TODO: TyBinder"

viewTopInfo : CodeTab -> TopBindingInfo -> Html CodeTabMsg
viewTopInfo tab ti = HtmlHelpers.list
    [ text ("hash: " ++ String.fromInt ti.topBindingHash)
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




