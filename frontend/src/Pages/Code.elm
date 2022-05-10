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

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

init : Cmd Msg
init = Cmd.batch [C.fetchProjectMeta, C.fetchPhase "Main" 0]


view : Model -> Html Msg
view model = div [] [ node "link" [rel "stylesheet", href "/style.css", type_ "text/css"] []
                    , node "link" [rel "stylesheet", href "/pygments.css", type_ "text/css"] []
                    , Loading.renderLoading "ProjectMeta" model.projectMetaLoading <| \meta ->
                        select [onInput (\x -> MsgLoadModule x 0)]
                               (List.map (\m -> option [] [text m.name]) meta.modules)

                    , Loading.renderLoading "Module" model.moduleLoading <| \mod -> 
                        div []
                        [ viewHeader model mod
                        , panel [ viewCode model mod
                                , viewInfo model
                                ]
                        ]
                    ]

panel : List (Html Msg) -> Html Msg
panel = div [ style "display" "grid"
            , style "width" "100%"
            , style "grid-template-columns" "4fr 1fr"
            ]

selectedTermId : Model -> Maybe Int
selectedTermId model = Maybe.map selectedTermToInt model.selectedTerm


viewHeader : Model -> Module -> Html Msg
viewHeader _ mod = 
    div []
        [ h1 [] [ text (String.fromInt mod.modulePhaseId ++ ". " ++ mod.moduleName.getModuleName ++ " -- " ++ mod.modulePhase) ]
        , button [onClick (MsgPrevPhase mod)] [text "Previous"]
        , button [onClick (MsgNextPhase mod)] [text "Next"]
        ]



viewCode : Model -> Module -> Html Msg
viewCode model mod = 
    let ppInfo = PP.defaultInfo 
            |> \r -> {r | selectId = Maybe.map selectedTermToInt model.selectedTerm}
            |> if model.disambiguateVariables then PP.withFullNameBinder else identity
    in pre [class "code"] (
         (if model.hideTypes then eraseTypesModule mod else mod)
         |> .moduleTopBindings
         |> List.map PP.ppTopBinding
         |> PP.ppSepped "\n\n"
         |> PP.prettyPrint ppInfo
         )

fromMaybe : a -> Maybe a -> a
fromMaybe def m = case m of
    Just x -> x
    Nothing -> def

viewInfo : Model -> Html Msg
viewInfo model = div [class "info-panel"]
                    [ h1 [] [text "Menu"]
                    , hr [] []
                    , h2 [] [text "ViewSettings"]
                    , HtmlHelpers.list 
                          [ checkbox model.hideTypes MsgToggleHideTypes "Hide Types"
                          , checkbox model.disambiguateVariables MsgToggleDisambiguateVariables "Disambiguate Variables Names"
                          ]
                    , hr [] []
                    , fromMaybe (h3 [] [text "No term selected"]) (Maybe.map viewTermInfo model.selectedTerm)
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



