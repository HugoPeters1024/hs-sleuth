module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Types exposing (..)
import Http
import Generated.HsCore as H
import HsCore.Trafo as Trafo
import PrettyPrint as PP

main : Program () Model Msg
main = Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }




subscriptions : Model -> Sub Msg
subscriptions _ = Sub.none

init : () -> (Model, Cmd Msg)
init _ = (Model Loading, fetchTest)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
    GotModule res -> ({ model | moduleLoading = loadFromResult res}, Cmd.none)

view : Model -> Html Msg
view model =
  div [] [ node "link" [rel "stylesheet", href "style.css", type_ "text/css"] []
         , node "link" [rel "stylesheet", href "pygments.css", type_ "text/css"] []
         , liftLoading viewCode model.moduleLoading ]


viewCode : H.Module -> Html Msg
viewCode mod = pre [class "code"]
                   (PP.runPP PP.defaultState (PP.ppSepped "\n\n" (List.map PP.ppTopBinding (Trafo.eraseTypesModule mod).moduleTopBindings)))


fetchTest : Cmd Msg
fetchTest = Http.get { url = "http://localhost:8080/output.json"
                     , expect = Http.expectJson GotModule H.moduleDecoder
                     }
