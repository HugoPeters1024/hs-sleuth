module Pages.Code exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import HtmlHelpers exposing (..)

import Generated.Types exposing (..)

import Types exposing (..)
import HsCore.Helpers as H

import HsCore.Trafo.Reconstruct as TR
import HsCore.Trafo.EraseTypes exposing (eraseTypesModule)

import PrettyPrint as PP
import Commands as C

import Loading exposing (Loading(..))
import Commands

mkCodeMsg : CodeTabMsg -> TabId -> Msg
mkCodeMsg msg id = MsgCodeMsg id msg

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

init : Cmd Msg
init = Cmd.batch [C.fetchProjectMeta, C.fetchCodePhase 0 "Main" 0]

update : CodeTabMsg -> CodeTab -> (CodeTab, Cmd Msg)
update msg tab = case msg of
    CodeMsgLoadModule mod phaseid -> ({tab | moduleLoading = Loading.setLoading tab.moduleLoading}, Commands.fetchCodePhase tab.id mod phaseid)
    CodeMsgGotModule mod -> ({tab | moduleLoading = Loading.loadFromResult mod}, Cmd.none)
    CodeMsgSelectTerm term -> ({tab | selectedTerm = Just term}, Cmd.none)
    CodeMsgNextPhase mod -> (tab, Commands.fetchCodeModifyPhase tab.id (\x -> x + 1) mod)
    CodeMsgPrevPhase mod -> (tab, Commands.fetchCodeModifyPhase tab.id (\x -> x - 1) mod)
    CodeMsgToggleHideTypes -> ({tab | hideTypes = not tab.hideTypes}, Cmd.none)
    CodeMsgToggleDisambiguateVariables -> ({tab | disambiguateVariables = not tab.disambiguateVariables}, Cmd.none)


view : Model -> CodeTab -> Html Msg
view model tab = 
    div [] [ node "link" [rel "stylesheet", href "/style.css", type_ "text/css"] []
           , node "link" [rel "stylesheet", href "/pygments.css", type_ "text/css"] []
           , Loading.renderLoading "ProjectMeta" model.projectMetaLoading <| \meta ->
               select [onInput (\x -> mkCodeMsg (CodeMsgLoadModule x 0) tab.id)]
                      (List.map (\m -> option [] [text m.name]) meta.modules)

           , Loading.renderLoading "Module" tab.moduleLoading <| \mod -> 
               div []
               [ viewHeader model tab mod
               , panel [ viewCode model tab mod
                       , viewInfo model tab
                       ]
               ]
           ]

panel : List (Html Msg) -> Html Msg
panel = div [ style "display" "grid"
            , style "width" "100%"
            , style "grid-template-columns" "4fr 1fr"
            ]

selectedTermId : CodeTab -> Maybe Int
selectedTermId tab = Maybe.map selectedTermToInt tab.selectedTerm


viewHeader : Model -> CodeTab -> Module -> Html Msg
viewHeader _ tab mod = 
    div []
        [ h3 [] [ text (String.fromInt mod.modulePhaseId ++ ". " ++ mod.moduleName.getModuleName ++ " -- " ++ mod.modulePhase) ]
        , button [onClick (mkCodeMsg (CodeMsgPrevPhase mod) tab.id)] [text "Previous"]
        , button [onClick (mkCodeMsg (CodeMsgNextPhase mod) tab.id)] [text "Next"]
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



