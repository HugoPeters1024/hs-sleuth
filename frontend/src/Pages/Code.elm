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
import HsCore.Trafo.Reconstruct as TR
import HsCore.Trafo.EraseTypes exposing (eraseTypesModule)
import PrettyPrint as PP
import Commands as C
import Loading exposing (Loading(..))
import Commands

import Set exposing (Set)

import UI.Slider as Slider

import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Dropdown  as Dropdown
import Bootstrap.Button  as Button

mkCodeMsg : CodeTabMsg -> TabId -> Msg
mkCodeMsg msg id = MsgCodeMsg id msg

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

initCodeTabModule : ProjectMeta -> CodeTabModule
initCodeTabModule meta = 
    { mod = Loading Nothing
    , projectMeta = meta
    , phaseSlider = Slider.init 0
    , topNames = []
    }

getProjectMetas : CodeTab -> List ProjectMeta
getProjectMetas tab = List.map .projectMeta (Dict.values tab.modules)

getMergedModuleNames : CodeTab -> List String
getMergedModuleNames tab = List.map .name (List.concatMap .modules (getProjectMetas tab))

getMergedTopBinders : CodeTab -> List Binder
getMergedTopBinders tab = List.concatMap .topNames (Dict.values tab.modules)


makeCodeTab : Model -> List ProjectMeta -> (Model, CodeTab, Cmd Msg)
makeCodeTab model metas = 
    let tabId = model.idGen
        slugs = List.map .slug metas
    in
    ( { model | idGen = model.idGen + 1 }
    , { id = tabId
      , name = "Code-" ++ String.fromInt tabId
      , modules = Dict.fromList (List.map (\m -> (m.slug, initCodeTabModule m)) metas)
      , currentModule = "Main"
      , selectedTerm = Nothing
      , hideTypes = False
      , disambiguateVariables = False
      , moduleDropdown = Dropdown.initialState
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
                    topNames = Loading.withDefault [] (Loading.map H.getModuleBinders mod)
                in {tabmod | mod = mod, topNames = topNames }
        in ({tab | modules = Dict.update slug (Maybe.map updateModuleTab) tab.modules}, Cmd.none)
    CodeMsgSelectTerm term -> ({tab | selectedTerm = Just term}, Cmd.none)
    CodeMsgToggleHideTypes -> ({tab | hideTypes = not tab.hideTypes}, Cmd.none)
    CodeMsgToggleDisambiguateVariables -> ({tab | disambiguateVariables = not tab.disambiguateVariables}, Cmd.none)
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
selectedTermId tab = Maybe.map selectedTermToInt tab.selectedTerm


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
            |> \r -> {r | selectId = Maybe.map selectedTermToInt tab.selectedTerm}
            |> if tab.disambiguateVariables then PP.withFullNameBinder else identity
    in div []
        [ h4 [] [text slug]
        , Loading.renderLoading modtab.mod (\mod -> text mod.modulePhase)
        , Slider.config
            { lift = \msg -> mkCodeMsg (CodeMsgSlider slug msg) tab.id
            , mininum = 0
            , maximum = case EH.find (\x -> x.name == tab.currentModule) modtab.projectMeta.modules of
                Just x -> x.nrPasses
                Nothing -> 0
            }
            |> Slider.view modtab.phaseSlider
        , pre [class "dark"] 
            [ code [] 
                   [Loading.renderLoading modtab.mod <| \mod ->
                       (
                         (if tab.hideTypes then eraseTypesModule mod else mod)
                         |> .moduleTopBindings
                         |> List.map PP.ppTopBinding
                         |> PP.ppSepped "\n\n"
                         |> PP.prettyPrint ppInfo
                         |> \x -> div [] x
                       )
                   ]
            ]
        ]

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
              , hr [] []
              , h4 [] [text "Selected Variable"]
              , fromMaybe (h5 [] [text "No term selected"]) (Maybe.map viewTermInfo tab.selectedTerm)
              , hr [] []
              , h4 [] [text "Toplevel functions"]
              , HtmlHelpers.list (List.map (text << H.binderName) (List.filter H.isSrcBinder (getMergedTopBinders tab)))
              ]
        ]
    |> Card.view

viewTermInfo : SelectedTerm -> Html Msg
viewTermInfo term = case term of
    SelectedBinder b -> viewTermBinder b
    SelectedTopLevel (b,s) -> div [] [viewTermBinder b, br [] [], text (Debug.toString s)]
    SelectedExternal (ExternalName e) -> p [] [ text (H.typeToString e.externalType) ]
    SelectedExternal ForeignCall -> p [] [text "ForeignCall"]

viewTermBinder : Binder -> Html Msg
viewTermBinder bndr = HtmlHelpers.list
            [ text ("name: " ++ H.binderName bndr)
            , text ("type: " ++ H.typeToString (H.binderType bndr))
            , text ("span: " ++ Debug.toString (H.binderSpan bndr))
            ]



