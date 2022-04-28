module HtmlHelpers exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

checkbox : Bool -> msg -> String -> Html msg
checkbox isChecked msg name =
    label
        [ ]
        [ input [ type_ "checkbox", checked isChecked, onClick msg ] []
        , text name
        ]

list : List (Html msg) -> Html msg
list xs =
    let mkColumn el = td [] [el]
        mkRow col = tr [] [mkColumn col]
    in table [] (List.map mkRow xs)
