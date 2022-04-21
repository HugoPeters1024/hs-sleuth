module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import HtmlHelpers exposing (..)

import Generated.Types exposing (..)

import Types exposing (..)
import HsCore.Helpers as H

import HsCore.Trafo.Reconstruct as TR
import HsCore.Trafo.EraseTypes exposing (eraseTypesModule)

import PrettyPrint as PP
import Loading exposing (Loading(..))
import Commands as C

main : Program () Model Msg
main = Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }

subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

initModel : Model
initModel = { projectMetaLoading = Loading Nothing
            , moduleLoading = Loading Nothing
            , selectedTerm = Nothing
            , hideTypes = False
            }

init : () -> (Model, Cmd Msg)
init _ = (initModel, Cmd.batch [C.fetchProjectMeta, C.fetchPhase "Main" 0])

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    MsgGotProjectMeta res -> ({model | projectMetaLoading = Loading.loadFromResult res}, Cmd.none)
    MsgLoadModule mod id -> ({model | moduleLoading = Loading.setLoading model.moduleLoading}, C.fetchPhase mod id)
    MsgGotModule res -> ({ model | moduleLoading = Loading.loadFromResult (Result.map TR.reconModule res)}, Cmd.none)
    MsgSelectTerm term -> ({model | selectedTerm = Just term}, Cmd.none)
    MsgNextPhase mod -> (model, C.fetchModifyPhase (\x->x + 1) mod)
    MsgPrevPhase mod -> (model, C.fetchModifyPhase (\x->x - 1) mod)
    MsgViewSettingsToggleHideTypes -> ({ model | hideTypes = not model.hideTypes }, Cmd.none)

view : Model -> Html Msg
view model =
  div [] [ node "link" [rel "stylesheet", href "style.css", type_ "text/css"] []
         , node "link" [rel "stylesheet", href "pygments.css", type_ "text/css"] []
         , Loading.loadOrDebug model.projectMetaLoading (text << Debug.toString)
         , Loading.liftLoading model.moduleLoading (text (Debug.toString model.moduleLoading)) <| \mod -> 
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
viewCode model mod = pre [class "code"] (
                     (if model.hideTypes then eraseTypesModule mod else mod)
                     |> .moduleTopBindings
                     |> List.map PP.ppTopBinding
                     |> PP.ppSepped "\n\n"
                     |> PP.prettyPrint (PP.defaultInfo (selectedTermId model))
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
                    , checkbox model.hideTypes MsgViewSettingsToggleHideTypes "Hide Types"
                    , hr [] []
                    , fromMaybe (h3 [] [text "No term selected"]) (Maybe.map viewTermInfo model.selectedTerm)
                    ]

viewTermInfo : SelectedTerm -> Html Msg
viewTermInfo term = div []
                          [ h3 [] [text "Selected term"]
                          , p [] [text "details:"]
                          , p [] [text (Debug.toString term)]
                          , case term of
                              SelectedBinder b -> p [] [text (H.typeToString (H.binderType b))]
                              SelectedExternal (ExternalName e) -> p [] [ text (H.typeToString e.externalType) ]
                              SelectedExternal ForeignCall -> p [] [text "ForeignCall"]
                          ]

