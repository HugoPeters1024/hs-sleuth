module Pages.Code exposing (..)

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
import Bootstrap.Grid as Grid

mkCodeMsg : CodeTabMsg -> TabId -> Msg
mkCodeMsg msg id = MsgCodeMsg id msg

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

makeCodeTab : Model -> List Slug -> (Model, CodeTab, Cmd Msg)
makeCodeTab model slugs = 
    let tabId = model.idGen
    in
    ( { model | idGen = model.idGen + 1 }
    , { id = tabId
      , name = "Code-" ++ String.fromInt tabId
      , modules = Dict.fromList (List.map (\s -> (s, Loading Nothing)) slugs)
      , selectedTerm = Nothing
      , hideTypes = False
      , disambiguateVariables = False
      , currentModule = "Main"
      , currentPhaseId = 0
      }
    , Cmd.batch (List.map (\slug -> C.fetchCodePhase tabId slug "Main" 0) slugs)
    )

init : Cmd Msg
init = Cmd.batch [C.fetchProjectMeta "secret", C.fetchCodePhase 0 "secret" "Main" 0, C.fetchCodePhase 0 "notsecret" "Main" 0]

update : CodeTabMsg -> CodeTab -> (CodeTab, Cmd Msg)
update msg tab = case msg of
    CodeMsgSetModule modname phaseid -> 
        ( {tab | modules = Dict.map (\_ -> Loading.setLoading) tab.modules 
               , currentModule = modname
               , currentPhaseId = phaseid
          }
        , Cmd.batch 
            ( List.map
             (\slug -> Commands.fetchCodePhase tab.id slug modname phaseid)
             (Dict.keys tab.modules)
            )
        )
    CodeMsgGotModule slug res -> ({tab | modules = Dict.insert slug (Loading.loadFromResult res) tab.modules}, Cmd.none)
    CodeMsgSelectTerm term -> ({tab | selectedTerm = Just term}, Cmd.none)
    CodeMsgToggleHideTypes -> ({tab | hideTypes = not tab.hideTypes}, Cmd.none)
    CodeMsgToggleDisambiguateVariables -> ({tab | disambiguateVariables = not tab.disambiguateVariables}, Cmd.none)


view : Model -> CodeTab -> Html Msg
view model tab = 
    div [] [ Loading.renderLoading "ProjectMeta" model.projectMetaLoading <| \meta ->
               select [onInput (\x -> mkCodeMsg (CodeMsgSetModule x 0) tab.id)]
                      (List.map (\m -> option [] [text m.name]) meta.modules)
           , panel
                (
                    (foreach (Dict.toList tab.modules) <| \(slug, mod) ->
                        (4, Loading.renderLoading "Code" mod <| \m -> viewCode model tab m)
                    )
                    ++
                    [ (2, viewInfo model tab) ]
                )
           ]


selectedTermId : CodeTab -> Maybe Int
selectedTermId tab = Maybe.map selectedTermToInt tab.selectedTerm


viewHeader : Model -> CodeTab -> Html Msg
viewHeader _ tab = 
    div []
        [ h3 []  [ text tab.currentModule]
        , button [] [text "Previous"]
        , button [] [text "Next"]
        ]



viewCode : Model -> CodeTab -> Module -> Html Msg
viewCode model tab mod = 
    let ppInfo = PP.defaultInfo tab.id
            |> \r -> {r | selectId = Maybe.map selectedTermToInt tab.selectedTerm}
            |> if tab.disambiguateVariables then PP.withFullNameBinder else identity
    in pre [class "dark"] 
           [ code [] (
                     (if tab.hideTypes then eraseTypesModule mod else mod)
                     |> .moduleTopBindings
                     |> List.map PP.ppTopBinding
                     |> PP.ppSepped "\n\n"
                     |> PP.prettyPrint ppInfo
                     )
           ]

fromMaybe : a -> Maybe a -> a
fromMaybe def m = case m of
    Just x -> x
    Nothing -> def

viewInfo : Model -> CodeTab -> Html Msg
viewInfo model tab = 
    div [ class "info-panel" ]
        [ h1 [] [text "Menu"]
        , hr [] []
        , h2 [] [text "ViewSettings"]
        , HtmlHelpers.list 
              [ checkbox tab.hideTypes (mkCodeMsg CodeMsgToggleHideTypes tab.id) "Hide Types"
              , checkbox tab.disambiguateVariables (mkCodeMsg CodeMsgToggleDisambiguateVariables tab.id) "Disambiguate Variables Names"
              ]
        , hr [] []
        , fromMaybe (h3 [] [text "No term selected"]) (Maybe.map viewTermInfo tab.selectedTerm)
        ]

viewTermInfo : SelectedTerm -> Html Msg
viewTermInfo term = div []
                          [ h3 [] [text "Selected term"]
                          --, p [] [text "details:"]
                          --, p [] [text (Debug.toString term)]
                          , case term of
                              SelectedBinder b -> viewTermBinder b
                              SelectedTopLevel (b,s) -> div [] [viewTermBinder b, br [] [], text (Debug.toString s)]
                              SelectedExternal (ExternalName e) -> p [] [ text (H.typeToString e.externalType) ]
                              SelectedExternal ForeignCall -> p [] [text "ForeignCall"]
                          ]

viewTermBinder : Binder -> Html Msg
viewTermBinder bndr = HtmlHelpers.list
            [ text ("name: " ++ H.binderName bndr)
            , text ("type: " ++ H.typeToString (H.binderType bndr))
            , text ("span: " ++ Debug.toString (H.binderSpan bndr))
            ]



