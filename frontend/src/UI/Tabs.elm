module UI.Tabs exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.Tab as BTab

type alias Model = BTab.State

type Msg = TabMsg Model

type alias Item msg =
    { name : String
    , content : Html msg
    }

toBTab : Item msg -> BTab.Item msg
toBTab it = BTab.item
    { id = it.name
    , link = BTab.link [] [text it.name]
    , pane = BTab.pane [] [it.content]
    }

init : Model
init = BTab.initialState

update : Msg -> Model -> Model
update action model = 
    case action of
        TabMsg tab -> tab


config : (Msg -> msg) -> BTab.Config msg
config lift = BTab.config (lift << TabMsg)

setTab : String -> Model -> Model
setTab tab _ = BTab.customInitialState tab

items = BTab.items

item = toBTab

view = BTab.view

link = BTab.link
