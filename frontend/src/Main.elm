module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Either exposing (Either(..))
import Types exposing (..)
import Http
import Generated.Types as H
import Generated.Decoders as HE
import HsCore.Trafo as Trafo
import HsCore.Helpers as H
import PrettyPrint as PP

main : Program () Model Msg
main = Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }




subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

initModel : Model
initModel = { moduleLoading = Loading Nothing
            , selectedTerm = Nothing
            }

init : () -> (Model, Cmd Msg)
init _ = (initModel, fetchPass "Main" 0)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    MsgGotModule res -> ({ model | moduleLoading = loadFromResult res}, Cmd.none)
    MsgSelectTerm term -> ({model | selectedTerm = Just term}, Cmd.none)
    MsgLoadModule mod id -> ({model | moduleLoading = setLoading model.moduleLoading}, fetchPass mod id)

view : Model -> Html Msg
view model =
  div [] [ node "link" [rel "stylesheet", href "style.css", type_ "text/css"] []
         , node "link" [rel "stylesheet", href "pygments.css", type_ "text/css"] []
         , liftLoading model.moduleLoading <| \mod -> 
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


viewHeader : Model -> H.Module -> Html Msg
viewHeader _ mod = 
    div []
        [ h1 [] [ text (String.fromInt mod.modulePhaseId ++ ". " ++ mod.moduleName ++ " -- " ++ mod.modulePhase) ]
        , button [onClick (MsgLoadModule mod.moduleName (mod.modulePhaseId - 1))] [text "Previous"]
        , button [onClick (MsgLoadModule mod.moduleName (mod.modulePhaseId + 1))] [text "Next"]
        ]



viewCode : Model -> H.Module -> Html Msg
viewCode model mod = pre [class "code"]
                     ( mod.moduleTopBindings
                     |> List.map PP.ppTopBinding
                     |> PP.ppSepped "\n\n"
                     |> PP.prettyPrint (PP.defaultInfo mod (selectedTermId model))
                     )

fromMaybe : a -> Maybe a -> a
fromMaybe def m = case m of
    Just x -> x
    Nothing -> def

viewInfo : Model -> Html Msg
viewInfo mod = div [class "info-panel"]
                    [ h1 [] [text "Menu"]
                    , fromMaybe (h3 [] [text "No term selected"]) (Maybe.map viewTermInfo mod.selectedTerm)
                    ]

viewTermInfo : SelectedTerm -> Html Msg
viewTermInfo term = div []
                          [ h3 [] [text "Selected term"]
                          , p [] [text "details:"]
                          , p [] [text (Debug.toString term)]
                          , case term of
                              SelectedBinder b -> p [] [text b.typeStr]
                              SelectedExternal _ -> p [] []
                          ]


fetchPass : String -> Int -> Cmd Msg
fetchPass mod id = Http.get { url = "http://localhost:8080/" ++ mod ++ "/" ++ String.fromInt id
                            , expect = Http.expectJson MsgGotModule HE.moduleDecoder
                            }
