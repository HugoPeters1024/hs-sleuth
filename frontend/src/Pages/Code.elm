module Pages.Code exposing (..)

import ElmHelpers as EH

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import HtmlHelpers exposing (..)
import Dict exposing (Dict)
import Generated.Types exposing (..)
import Types exposing (..)
import HsCore.Helpers as H
import HsCore.Trafo.EraseTypes exposing (eraseTypesModule)
import HsCore.Trafo.Diff as Diff
import PrettyPrint as PP
import Commands as C
import Loading exposing (Loading(..))
import Commands

import ContextMenu

import Set exposing (Set)

import UI.Slider as Slider

import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Dropdown  as Dropdown
import Bootstrap.Button  as Button

mkCodeMsg : CodeTabMsg -> TabId -> Msg
mkCodeMsg msg id = MsgCodeMsg id msg

subscriptions : CodeTab -> Sub Msg
subscriptions tab = Sub.map (MsgCodeMsg tab.id) (Dropdown.subscriptions tab.moduleDropdown CodeMsgModuleDropdown)

initCodeTabModule : Capture -> CodeTabModule
initCodeTabModule meta = 
    { mod = Loading Nothing
    , projectMeta = meta
    , phaseSlider = Slider.init 0
    , topNames = []
    }

getCaptures : CodeTab -> List Capture
getCaptures tab = List.map .projectMeta (Dict.values tab.modules)

getMergedModuleNames : CodeTab -> List String
getMergedModuleNames tab = List.map Tuple.first (List.concatMap .captureModules (getCaptures tab))
    |> EH.removeDuplicates

getMergedTopBinders : CodeTab -> List TopBindingInfo
getMergedTopBinders tab = List.concatMap .topNames (Dict.values tab.modules)
    |> EH.removeDuplicatesKey (H.binderName << .topBindingBinder)



makeCodeTab : Model -> List Capture -> (Model, CodeTab, Cmd Msg)
makeCodeTab model captures = 
    let tabId = model.idGen
        slugs = List.map .captureName captures
    in
    ( { model | idGen = model.idGen + 1 }
    , { id = tabId
      , name = "Code-" ++ String.fromInt tabId
      , modules = Dict.fromList (List.map (\m -> (m.captureName, initCodeTabModule m)) captures)
      , currentModule = "Main"
      , selectedVar = Nothing
      , moduleDropdown = Dropdown.initialState
      , hideTypes = False
      , disambiguateVariables = False
      , showRecursiveGroups = False
      , selectedTopLevels = []
      }
    , Cmd.batch (List.map (\slug -> C.fetchCodePhase tabId slug "Main" 0) slugs)
    )

update : CodeTabMsg -> CodeTab -> (CodeTab, Cmd Msg)
update msg tab = case msg of
    CodeMsgSetModule modname phaseid -> 
        ( {tab | currentModule = modname }
        , Cmd.batch 
            ( List.map
             (\slug -> Commands.fetchCodePhase tab.id slug modname phaseid)
             (Dict.keys tab.modules)
            )
        )
    CodeMsgGotModule slug res -> 
        let updateModuleTab : CodeTabModule -> CodeTabModule 
            updateModuleTab tabmod = 
                let mod = Loading.loadFromResult res
                    topNames = Loading.withDefault [] (Loading.map H.getModuleTopBinders mod)
                in {tabmod | mod = mod, topNames = topNames }
        in ({tab | modules = Dict.update slug (Maybe.map updateModuleTab) tab.modules}, Cmd.none)
    CodeMsgSelectVar var -> ({tab | selectedVar = Just var}, Cmd.none)
    CodeMsgToggleHideTypes -> ({tab | hideTypes = not tab.hideTypes}, Cmd.none)
    CodeMsgToggleDisambiguateVariables -> ({tab | disambiguateVariables = not tab.disambiguateVariables}, Cmd.none)
    CodeMsgToggleShowRecursiveGroups -> ({tab | showRecursiveGroups = not tab.showRecursiveGroups}, Cmd.none)
    CodeMsgModuleDropdown state -> ({tab | moduleDropdown = state}, Cmd.none)
    CodeMsgSlider slug slidermsg ->
        let updateModuleTab : CodeTabModule -> CodeTabModule
            updateModuleTab tabmod = {tabmod | phaseSlider = Slider.update slidermsg tabmod.phaseSlider }

            newtab : CodeTab
            newtab = {tab | modules = Dict.update slug (Maybe.map updateModuleTab) tab.modules}
        in ( newtab
           , case Dict.get slug newtab.modules of
               Nothing -> Cmd.none
               Just modtab -> Commands.fetchCodePhase tab.id slug tab.currentModule modtab.phaseSlider.value
           )
    CodeMsgMarkTopLevel ti -> ({tab | selectedTopLevels = ti::tab.selectedTopLevels }, Cmd.none)

view : Model -> CodeTab -> Html Msg
view model tab = 
    div [] [ viewHeader model tab
           , panel
                (
                    (foreach (Dict.toList tab.modules) <| \(slug, mod) ->
                        (Fraction 4, viewCode model tab slug mod)
                    )
                    ++
                    [ (Pixels 500, viewInfo model tab) ]
                )
           ]


selectedTermId : CodeTab -> Maybe Int
selectedTermId tab = Maybe.map H.varToInt tab.selectedVar


viewHeader : Model -> CodeTab -> Html Msg
viewHeader _ tab = 
    div []
        [ h3 []  [text tab.currentModule]
        , Dropdown.dropdown tab.moduleDropdown
            { options = []
            , toggleMsg = \s -> mkCodeMsg (CodeMsgModuleDropdown s) tab.id
            , toggleButton = Dropdown.toggle [Button.primary] [text "Module"]
            , items = List.map 
                (\modname -> Dropdown.buttonItem [onClick (mkCodeMsg (CodeMsgSetModule modname 0) tab.id)] [text modname]) 
                (getMergedModuleNames tab)
            }
        ]



viewCode : Model -> CodeTab -> Slug -> CodeTabModule -> Html Msg
viewCode model tab slug modtab = 
    let ppInfo = PP.defaultInfo tab.id
            |> \r1 -> {r1 | selectedVar = tab.selectedVar}
            |> \r2 -> {r2 | renderVarAttributes = \var -> [ContextMenu.open MsgCtxMenu (OnTerm (H.varName False var))]}
            |> if tab.disambiguateVariables then PP.withFullNameBinder else identity
    in div []
        [ h4 [] [text slug]
        , Loading.renderLoading modtab.mod (\mod -> text mod.modulePhase)
        , Slider.config
            { lift = \msg -> mkCodeMsg (CodeMsgSlider slug msg) tab.id
            , mininum = 0
            , maximum = case EH.find (\(name,_) -> name == tab.currentModule) modtab.projectMeta.captureModules of
                Just (_, nrPasses) -> nrPasses
                Nothing -> 0
            }
            |> Slider.view modtab.phaseSlider
        , pre [class "dark"] 
            [ code [] 
                   [Loading.renderLoading modtab.mod <| \mod ->
                       (
                         processDiff tab mod
                         |> (if tab.hideTypes then eraseTypesModule else identity)
                         |> (if tab.showRecursiveGroups then identity else \m -> {m | moduleTopBindings = H.removeRecursiveGroups m.moduleTopBindings})
                         |> .moduleTopBindings
                         |> List.map PP.ppTopBinding
                         |> PP.ppSepped "\n\n"
                         |> PP.prettyPrint ppInfo
                         |> \x -> div [] x
                       )
                   ]
            ]
        ]

processDiff : CodeTab -> Module -> Module
processDiff tab mod = case tab.selectedTopLevels of
    [lhs, rhs] -> 
        let go : TopBindingInfo -> TopBindingInfo
            go tb = 
                if H.topBindingInfoToInt tb == H.topBindingInfoToInt lhs && H.binderPhaseId tb.topBindingBinder == H.binderPhaseId lhs.topBindingBinder
                then Tuple.first (Diff.anotateTopBindingInfo (lhs, rhs)) 
                else (
                    if H.topBindingInfoToInt tb == H.topBindingInfoToInt rhs && H.binderPhaseId tb.topBindingBinder == H.binderPhaseId rhs.topBindingBinder
                    then Tuple.second (Diff.anotateTopBindingInfo (lhs, rhs))
                    else tb
                )
                    
        in { mod | moduleTopBindings = List.map (H.topBindingMap go) mod.moduleTopBindings }
    _          -> mod

fromMaybe : a -> Maybe a -> a
fromMaybe def m = case m of
    Just x -> x
    Nothing -> def

viewInfo : Model -> CodeTab -> Html Msg
viewInfo model tab = 
    Card.config []
    |> Card.headerH3 [] [text "Options"]
    |> Card.block []
        [ Block.titleH4 [] [text "View Options"]
        , Block.custom <|
            HtmlHelpers.list 
              [ checkbox tab.hideTypes (mkCodeMsg CodeMsgToggleHideTypes tab.id) "Hide Types"
              , checkbox tab.disambiguateVariables (mkCodeMsg CodeMsgToggleDisambiguateVariables tab.id) "Disambiguate Variables Names"
              , checkbox tab.showRecursiveGroups   (mkCodeMsg CodeMsgToggleShowRecursiveGroups   tab.id) "Show Recursive Groups"
              , hr [] []
              , h4 [] [text "Selected Variable"]
              , fromMaybe (h5 [] [text "No term selected"]) (Maybe.map (viewVarInfo tab) tab.selectedVar)
              , hr [] []
              , h4 [] [text "Toplevel functions"]
              , HtmlHelpers.list (List.map (text << H.binderName << .topBindingBinder) (List.filter .topBindingFromSource (getMergedTopBinders tab)))
              ]
        ]
    |> Card.view

viewVarInfo : CodeTab -> Var -> Html Msg
viewVarInfo tab term = case term of
    VarBinder b -> viewBinderInfo b
    VarTop tb -> viewTopInfo tab tb
    VarExternal e -> viewExternalInfo e


viewBinderInfo : Binder -> Html Msg
viewBinderInfo bndr = HtmlHelpers.list
            [ text ("name: " ++ H.binderName bndr)
            , text ("type: " ++ H.typeToString (H.binderType bndr))
            , text ("span: " ++ Debug.toString (H.binderSpan bndr))
            ]

viewTopInfo : CodeTab -> TopBindingInfo -> Html Msg
viewTopInfo tab ti = div []
    [ button [onClick (mkCodeMsg (CodeMsgMarkTopLevel ti) tab.id)] [text "Mark"]
    , text ("#markers: " ++ String.fromInt (List.length tab.selectedTopLevels))
    , viewBinderInfo ti.topBindingBinder
    ]

viewExternalInfo : ExternalName -> Html Msg
viewExternalInfo ext = case ext of
    ForeignCall -> text ("[ForeignCall]")
    ExternalName e -> HtmlHelpers.list
        [ text ("name: " ++ e.externalName)
        , text ("module:"  ++ e.externalModuleName)
        , text ("type: " ++ H.typeToString e.externalType)
        ]




