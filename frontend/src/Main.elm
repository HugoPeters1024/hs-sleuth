module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Either exposing (Either(..))
import Types exposing (..)
import Http
import Generated.HsCore as H
import HsCore.Trafo as Trafo
import HsCore.Helpers as H
import PrettyPrint as PP

main : Program () Model Msg
main = Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }




subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

initModel : Model
initModel = { moduleLoading = Loading
            , selectedTerm = Nothing
            }

init : () -> (Model, Cmd Msg)
init _ = (initModel, fetchTest)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    MsgGotModule res -> ({ model | moduleLoading = loadFromResult res}, Cmd.none)
    MsgSelectTerm term -> ({model | selectedTerm = Just term}, Cmd.none)

view : Model -> Html Msg
view model =
  div [] [ node "link" [rel "stylesheet", href "style.css", type_ "text/css"] []
         , node "link" [rel "stylesheet", href "pygments.css", type_ "text/css"] []
         , panel [ liftLoading (viewCode model) model.moduleLoading 
                 , viewInfo model
                 ]
         ]

panel : List (Html Msg) -> Html Msg
panel = div [ style "display" "grid"
            , style "width" "100%"
            , style "grid-template-columns" "4fr 1fr"
            ]

selectedTermId : Model -> Maybe Int
selectedTermId model =
    let go t = case t of
            Left binder -> H.binderToInt binder
            Right en -> H.externalNameToInt en
    in Maybe.map go model.selectedTerm





viewCode : Model -> H.Module -> Html Msg
viewCode model mod = pre [class "code"]
                     ( Trafo.eraseTypesModule mod
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
viewInfo mod = div [class "info-panel"]
                    [ h1 [] [text "Menu"]
                    , fromMaybe (h3 [] [text "No term selected"]) (Maybe.map viewTermInfo mod.selectedTerm)
                    ]

viewTermInfo : Either H.Binder H.ExternalName -> Html Msg
viewTermInfo binder = div []
                          [ h3 [] [text "Selected term"]
                          , p [] [text "details:"]
                          , p [] [text (Debug.toString binder)]
                          ]


fetchTest : Cmd Msg
fetchTest = Http.get { url = "http://localhost:8080/output.json"
                     , expect = Http.expectJson MsgGotModule H.moduleDecoder
                     }
