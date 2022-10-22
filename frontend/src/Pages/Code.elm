module Pages.Code exposing (..)

import ElmHelpers as EH

import Html exposing (..)
import Html.Lazy
import Html.Parser
import Html.Parser.Util
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import HtmlHelpers exposing (..)
import Dict exposing (Dict)
import Generated.Types exposing (..)
import Generated.Decoders
import Types exposing (..)
import HsCore.Helpers exposing (..)
import HsCore.Trafo.EraseTypes exposing (eraseTypesPhase)
import HsCore.Trafo.VarOccs exposing (exprVarOccs)
import HsCore.Trafo.Reconstruct as Recon

import Ppr as Ppr
import PprRender as Ppr

import Loading exposing (Loading(..))

import Set exposing (Set)
import Set.Any exposing (AnySet)

import UI.Slider as Slider

import Bootstrap.Card as Card
import Bootstrap.Alert as Alert
import Bootstrap.Card.Block as Block
import Bootstrap.Dropdown  as Dropdown
import Bootstrap.Button  as Button
import Bootstrap.Modal as Modal
import Bootstrap.Form.Input as Input
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row

import Zip.Entry
import Json.Decode

mkCodeMsg : TabId -> CodeTabMsg -> Msg
mkCodeMsg id msg = MsgCodeMsg id msg

subscriptions : CodeTab -> Sub Msg
subscriptions tab = Sub.map (MsgCodeMsg tab.id) (Dropdown.subscriptions tab.moduleDropdown CodeMsgModuleDropdown)

initCodeTabCapture : Int -> CaptureView -> ModuleName -> CodeTabCapture
initCodeTabCapture slot capture_view modname = 
    { phase = getPhaseFromView capture_view modname 0
    , src = getSrcFromView capture_view modname
    , capture_view = capture_view
    , phaseSlider = Slider.init 0
    , slot = slot
    , toplevelHides = Set.empty
    , srcToggle = Core
    }

getCurrentCaptures : CodeTab -> List CaptureView
getCurrentCaptures tab = List.map .capture_view (Dict.values tab.captureSlots)

getCurrentCaptureTabs : CodeTab -> List CodeTabCapture
getCurrentCaptureTabs tab = Dict.values tab.captureSlots

getPhases : CodeTab -> List Phase
getPhases = EH.mapResult .phase << Dict.values << .captureSlots

getMergedModuleNames : CodeTab -> List String
getMergedModuleNames tab = List.map Tuple.first (List.concatMap (.captureModules << .capture) (getCurrentCaptures tab))
    |> EH.removeDuplicates


getCommonToplevelIds : CodeTab -> AnySet Int TopBindingInfo
getCommonToplevelIds tab =
   let phaseSet : Phase -> AnySet Int TopBindingInfo
       phaseSet phase = getPhaseTopBinders phase |> Set.Any.fromList .topBindingHash
       
   in EH.anySetIntersectMany .topBindingHash (List.map phaseSet (getPhases tab))

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

  in Dict.values <| List.foldl go Dict.empty (List.map getPhaseTopBinders (getPhases tab))

makeCodeTab : Model -> List CaptureView -> (Model, CodeTab, Cmd Msg)
makeCodeTab model cvs = 
    let tabId = model.idGen
        currentModule = 
            List.head cvs
            |> Maybe.andThen (.capture >> .captureModules >> List.map Tuple.first >> List.head)
            |> Maybe.withDefault "Main"

        tab : CodeTab
        tab =
          { id = tabId
          , name = "Inspection" ++ String.fromInt tabId
          , captureSlots = Dict.fromList (List.map (\(i, c) -> (i, initCodeTabCapture i c currentModule)) (EH.enumerate cvs))
          , currentModule = currentModule
          , selectedVar = Nothing
          , varHighlights = Set.empty
          , moduleDropdown = Dropdown.initialState
          , codeViewOptions = 
            { hideTypes = False
            , hideModules = False
            , hideDisambiguation = True
            , hideRecursiveGroups = True
            , hideUndemanded = True
            , desugarLeadingLambdas = True
            , varRenames = Dict.empty
          }
          , selectedTopLevels = []
          , renameModal = 
              { visiblity = Modal.hidden
              , stagingText = ""
              , varId = -1
              }
          , inspectVar = Nothing
          , varHover = Nothing
          }
    in
    ( { model | idGen = model.idGen + 1 }
      , tab
      , Cmd.none
    )

readFileFromView : CaptureView -> String -> Result String String
readFileFromView cv filename =
  Dict.get filename cv.files
  |> Result.fromMaybe (filename ++ " is missing")
  |> Result.andThen (Zip.Entry.toString >> Result.mapError (\_ -> "could not read " ++ filename))


getPhaseFromView : CaptureView -> String -> Int -> Result String Phase
getPhaseFromView cv modname phase_id = 
  let filename = modname ++ "_" ++ String.fromInt phase_id ++ ".json"
  in readFileFromView cv filename
      |> Result.andThen (Json.Decode.decodeString Generated.Decoders.phaseDecoder >> Result.mapError (\_ -> "could not parse " ++ filename))
      |> Result.map Recon.reconPhase

getSrcFromView : CaptureView -> String -> Result String (Html msg)
getSrcFromView cv modname = 
  readFileFromView cv (modname ++ ".html")
  |> Result.andThen (\txt -> Html.Parser.run txt |> Result.mapError (\_ -> "Cannot parse the html src"))
  |> Result.map Html.Parser.Util.toVirtualDom
  |> Result.map (span [])

update : CodeTabMsg -> CodeTab -> (CodeTab, Cmd Msg)
update msg tab = case msg of
    CodeMsgSetModule modname ->
        let resetSlider : CodeTabCapture -> CodeTabCapture
            resetSlider modtab = { modtab | phaseSlider = Slider.init 0 }

            setPhase : CodeTabCapture -> CodeTabCapture
            setPhase modtab = { modtab | phase = getPhaseFromView modtab.capture_view modname 0 }

            setSrc : CodeTabCapture -> CodeTabCapture
            setSrc modtab = { modtab | src = getSrcFromView modtab.capture_view modname }

        in ({ tab | currentModule = modname, captureSlots = Dict.map (\_ -> resetSlider >> setPhase >> setSrc) tab.captureSlots }, Cmd.none)
    CodeMsgSetPhase slot phase_id -> 
        let setPhase : CodeTabCapture -> CodeTabCapture
            setPhase x = {x | phase = getPhaseFromView x.capture_view tab.currentModule phase_id, phaseSlider = Slider.init phase_id }
        in
        ( { tab | captureSlots = Dict.update slot (Maybe.map setPhase) tab.captureSlots }
        , Cmd.none
        )
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
    CodeMsgToggleDesugarLeadingLambdas -> ({tab | codeViewOptions = codeViewOptionsToggleDesugarLeadingLambdas tab.codeViewOptions}, Cmd.none)
    CodeMsgModuleDropdown state -> ({tab | moduleDropdown = state}, Cmd.none)
    CodeMsgSlider slot slidermsg ->
        let updateCaptureTab : CodeTabCapture -> CodeTabCapture
            updateCaptureTab tabmod = 
              let new_slider = Slider.update slidermsg tabmod.phaseSlider
              in
              { tabmod 
                | phaseSlider = new_slider
                , phase = getPhaseFromView tabmod.capture_view tab.currentModule new_slider.value
              }

        in ({tab | captureSlots = Dict.update slot (Maybe.map updateCaptureTab) tab.captureSlots}, Cmd.none)
    CodeMsgRenameModalOpen var -> ({tab | renameModal = renameModalOpen var tab.renameModal}, Cmd.none)
    CodeMsgRenameModalClose -> ({tab | renameModal = renameModalClose tab.renameModal}, Cmd.none)
    CodeMsgRenameModalStagingText txt -> ({tab | renameModal = renameModalSetStagingText txt tab.renameModal}, Cmd.none)
    CodeMsgRenameModalCommit -> 
        let varRenames = Dict.insert tab.renameModal.varId tab.renameModal.stagingText tab.codeViewOptions.varRenames
            modal = renameModalClose tab.renameModal
        in ({tab | codeViewOptions = codeViewOptionsMapVarRenames (\_ -> varRenames) tab.codeViewOptions, renameModal = modal}, Cmd.none)
    CodeMsgUnhideVar slot var -> 
        let updateHideSet : CodeTabCapture -> CodeTabCapture
            updateHideSet tabmod = {tabmod | toplevelHides = Set.remove (varToInt var) tabmod.toplevelHides}
        in ({tab | captureSlots = Dict.update slot (Maybe.map updateHideSet) tab.captureSlots}, Cmd.none)
    CodeMsgHideToplevel slot ti -> 
        let updateHideSet : CodeTabCapture -> CodeTabCapture
            updateHideSet tabmod = {tabmod | toplevelHides = EH.toggleSet (topBindingInfoToInt ti) tabmod.toplevelHides}
        in ({tab | captureSlots = Dict.update slot (Maybe.map updateHideSet) tab.captureSlots}, Cmd.none)
    CodeMsgHideToplevelAllBut slot ti -> 
      let allVarIds =
            getCurrentCaptures tab
            |> EH.mapMaybe (.module_metas >> Dict.get tab.currentModule)
            |> List.map .toplevels
            |> List.concatMap Dict.keys
            |> EH.mapMaybe String.toInt
            |> Set.fromList
            
          updateHideSet : CodeTabCapture -> CodeTabCapture
          updateHideSet tabmod = {tabmod | toplevelHides = Set.remove (topBindingInfoToInt ti) allVarIds}
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
            hideSet = getMatchedTopLevel .topBindingHash tab 
              |> List.filter pred
              |> List.concat
              |> List.map topBindingInfoToInt
              |> Set.fromList

            updateHideSet : CodeTabCapture -> CodeTabCapture
            updateHideSet tabmod = {tabmod | toplevelHides = Set.union tabmod.toplevelHides hideSet }
        in ({tab | captureSlots = Dict.map (\_ c -> updateHideSet c) tab.captureSlots}, Cmd.none)
    CodeMsgUnhideAll ->
      let updateHideSet : CodeTabCapture -> CodeTabCapture
          updateHideSet tabmod = {tabmod | toplevelHides = Set.empty}
      in ({tab | captureSlots = Dict.map (\_ -> updateHideSet) tab.captureSlots}, Cmd.none)
    CodeMsgHighlightVar var -> ({tab | varHighlights = EH.toggleSet (HsCore.Helpers.varToInt var) tab.varHighlights}, Cmd.none)
    CodeMsgRemoveAllHightlights -> ({tab | varHighlights = Set.empty}, Cmd.none)
    CodeMsgHoverVar var -> ({ tab | varHover = Just var}, Cmd.none)
    CodeMsgDehoverVar -> ({tab | varHover = Nothing}, Cmd.none)
    CodeMsgInspectVar var -> ({tab | inspectVar = Just var}, Cmd.none)
    CodeMsgInspectVarClose -> ({tab | inspectVar = Nothing}, Cmd.none)


view : Model -> CodeTab -> Html Msg
view model = Html.Lazy.lazy <| \tab ->
    div [] [ viewHeader model tab
           , panel
                (
                    (Pixels 300, Html.map (mkCodeMsg tab.id) (viewInfo model tab))
                    ::
                    (foreach (Dict.values tab.captureSlots) <| \mod ->
                        (Fraction 4, viewCode tab mod)
                    )
                )
           , Html.map (mkCodeMsg tab.id) <| viewRenameModal tab
           , Html.map (mkCodeMsg tab.id) <| viewInspectVarModal tab
           ]


selectedTermId : CodeTab -> Maybe Int
selectedTermId tab = Maybe.map varToInt tab.selectedVar


viewHeader : Model -> CodeTab -> Html Msg
viewHeader _ tab = 
    div []
        [ p [] []
        , Dropdown.dropdown tab.moduleDropdown
            { options = []
            , toggleMsg = \s -> mkCodeMsg tab.id (CodeMsgModuleDropdown s)
            , toggleButton = Dropdown.toggle [Button.primary] [text tab.currentModule]
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

viewInspectVarModal : CodeTab -> Html CodeTabMsg
viewInspectVarModal tab = case tab.inspectVar of
  Nothing -> text ""
  Just var ->
    let onClose = CodeMsgInspectVarClose

        description : String
        description = case var of
          VarBinder _ -> "A locally bound term"
          VarTop _ -> "A term defined in the current module at the top level"
          VarExternal ForeignCall -> "A call to foreign code"
          VarExternal (ExternalName e) -> "A term imported from the module " ++ e.externalModuleName

        source : String
        source = case HsCore.Helpers.varCreatedPhaseId var of
            Nothing -> "Build artifact of another module"
            Just id -> if id == 0 then "Desugared from the source code" else "Generated at phase " ++ (String.fromInt id)

        details = case HsCore.Helpers.varIdDetails var of
            Nothing   -> "Unknown"
            Just VanillaId -> "Vanilla"
            Just RecSelId -> "Record selector"
            Just DataConWorkId -> "Data constructor"
            Just DataConWrapId -> "Data constructor"
            Just ClassOpId -> "Class operation"
            Just PrimOpId -> "Primitive operation"
            Just TickBoxOpId -> "HPC tickbox"
            Just DFunId -> "Typeclass dictionary function"
            Just CoVarId -> "Coercion variable"
            Just (JoinId j) -> "Join point with arity " ++ String.fromInt j.joinIdArity

        renderRow : String -> String -> Html CodeTabMsg
        renderRow k v = 
          Grid.row [] 
            [ Grid.col [Col.xs3] [text k]
            , Grid.col [Col.xs] [text v]
            ]


    in Modal.config onClose
      |> Modal.large
      |> Modal.hideOnBackdropClick True
      |> Modal.h3 [] [text (HsCore.Helpers.varName var ++ " :: " ++ HsCore.Helpers.typeToString (HsCore.Helpers.varType var))]
      |> Modal.body [] 
          [ Grid.container []
              [ renderRow "Description" description
              , renderRow "Source"      source
              , renderRow "IdDetails"     details
              , hr [] []
              , case HsCore.Helpers.varIdInfo var of
                  Just id_info -> div []
                    [ h4 [] [text ("IdInfo (at phase " ++ (String.fromInt (HsCore.Helpers.varPhaseId var)) ++ ")")]
                    , renderRow "Arity" (String.fromInt id_info.idiArity)
                    , renderRow "Lambda with 1 usage" (EH.boolToString id_info.idiIsOneShot)
                    , renderRow "Has Core unfolding" (case id_info.idiUnfolding of
                        CoreUnfolding _ -> "yes"
                        _               -> "no"
                      )
                    , renderRow "Inline Pragma" id_info.idiInlinePragma
                    , renderRow "Occurances" (HsCore.Helpers.occInfoToString id_info.idiOccInfo)
                    , renderRow "Strictness Sig." id_info.idiStrictnessSig
                    , renderRow "Demand Sig." id_info.idiDemandSig
                    , renderRow "Call Arity" (String.fromInt id_info.idiCallArity)
                    ]
                  _ -> h4 [] [text "No IdInfo for this variable"]
              ]
          ]
      |> Modal.footer []
          [ Button.button
              [ Button.outlinePrimary
              , Button.attrs [onClick onClose]
              ] [text "Close"]
          ]
      |> Modal.view Modal.shown


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
        [ h4 [] [text modtab.capture_view.capture.captureName]
        , case modtab.phase of
             Err problem -> Alert.simpleDanger [] [text problem]
             Ok phase -> div []
                 [ text phase.phaseName
                 , Slider.config
                     { lift = \msg -> mkCodeMsg tab.id (CodeMsgSlider modtab.slot msg)
                     , mininum = 0
                     , maximum =
                        EH.find (\(x,_) -> x==tab.currentModule) modtab.capture_view.capture.captureModules
                        |> Maybe.map Tuple.second
                        |> Maybe.withDefault 0
                     }
                     |> Slider.view modtab.phaseSlider
                 , pre [class "dark"] 
                   [ case modtab.srcToggle of
                       Core -> code [] 
                                    [ a [class "src-toggle", onClick (mkCodeMsg tab.id (CodeMsgToggleSrc modtab.slot))] [text "view source\n\n"]
                                    , Ppr.dyn_css tab.varHighlights tab.selectedVar tab.varHover
                                    , Html.Lazy.lazy6 renderPhase tab.codeViewOptions tab.currentModule modtab.toplevelHides tab.id modtab.slot phase
                                    ]
                              
                       Src -> code []
                                   [ a [class "src-toggle", onClick (mkCodeMsg tab.id (CodeMsgToggleSrc modtab.slot))] [text "view core\n\n"]
                                   , case modtab.src of
                                        Err problem -> Alert.simpleDanger [] [text problem]
                                        Ok src -> src
                                   ]
                   ]
                 ]
        ]


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
              , checkbox tab.codeViewOptions.desugarLeadingLambdas CodeMsgToggleDesugarLeadingLambdas "Desugar Leading Lambdas"
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
