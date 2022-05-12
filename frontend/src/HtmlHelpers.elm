module HtmlHelpers exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Time

checkbox : Bool -> msg -> String -> Html msg
checkbox isChecked msg name =
    label
        [ ]
        [ input [ type_ "checkbox", checked isChecked, onClick msg ] []
        , text name
        ]

checkboxSimple : Bool -> Html msg
checkboxSimple isChecked = label [] [input [type_ "checkbox", checked isChecked] []]

list : List (Html msg) -> Html msg
list xs =
    let mkColumn el = td [] [el]
        mkRow col = tr [] [mkColumn col]
    in table [] (List.map mkRow xs)

renderDateTime : Time.Zone -> Time.Posix -> String
renderDateTime zone time = 
  let
    day   = String.fromInt (Time.toDay   zone time)
    month = String.fromInt (monthToInt (Time.toMonth zone time))
    year  = String.fromInt (Time.toYear zone time)

    hour   = String.fromInt (Time.toHour   zone time)
    minute = String.fromInt (Time.toMinute zone time)
    second = String.fromInt (Time.toSecond zone time)
  in day ++ "/" ++ month ++ "/" ++ year ++ " " ++ hour ++ ":" ++ minute ++ ":" ++ second

monthToInt : Time.Month -> Int
monthToInt month = case month of
    Time.Jan -> 1
    Time.Feb -> 2
    Time.Mar -> 3
    Time.Apr -> 4
    Time.May -> 5
    Time.Jun -> 6
    Time.Jul -> 7
    Time.Aug -> 8
    Time.Sep -> 9
    Time.Oct -> 10
    Time.Nov -> 11
    Time.Dec -> 12

foreach : List a -> (a -> b) -> List b
foreach xs f = List.map f xs

type Column 
    = Fraction Int
    | Pixels Int

panel : List ((Column, Html msg)) -> Html msg
panel data = 
    let (frs, els) = List.unzip data
        renderColumn c = case c of
            Fraction fr -> "minmax(0," ++ String.fromInt fr ++ "fr)"
            Pixels px -> String.fromInt px ++ "px"
        template = String.join " " (List.map renderColumn frs)
    in div [ style "display" "grid"
           , style "width" "100%"
           , style "grid-template-columns" template
           ] els 
