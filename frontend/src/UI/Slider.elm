module UI.Slider exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

type alias Model =
    { value : Int
    }

type alias ViewModel msg =
    { lift : Msg -> msg
    , mininum : Int
    , maximum : Int
    }



type Msg =
    SliderUpdate Model

init : Int -> Model
init v = { value = v}

update : Msg -> Model -> Model
update (SliderUpdate m) _ = m

config : ViewModel msg -> ViewModel msg
config = identity

view : Model -> ViewModel msg -> Html msg
view m vm = 
    div [style "width" "100%"]
    [
        input [ type_ "range"
              , Html.Attributes.min (String.fromInt vm.mininum)
              , Html.Attributes.max (String.fromInt vm.maximum)
              , value (String.fromInt m.value)
              , onInput (\inp -> vm.lift (case String.toInt inp of
                  Nothing -> SliderUpdate m
                  Just i -> SliderUpdate {m | value = i}))
              , style "width" "100%"
              , style "height" "24px"
              ] []
    ]
