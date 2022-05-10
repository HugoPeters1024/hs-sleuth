module Pages.Overview exposing (view, init)

import Html exposing (..)
import HtmlHelpers exposing (..)
import Types exposing (..)
import Loading exposing (Loading)
import Commands

import Bootstrap.Table as Table
import Time

init : Cmd Msg
init = Commands.fetchSessionMeta

view : Model -> Html Msg
view m = 
    let
        mkRow (name, project) = 
            Table.tr [] 
                [ Table.td [] [text name]
                    , Table.td [] [text (renderDateTime m.timezone (Time.millisToPosix project.capturedAt))]
                ] 
    in Loading.renderLoading "SessionMeta" m.sessionMetaLoading <| \session ->
            div []
                [ h1 [] [text "Overview"]
                , Table.table
                    { options = [Table.striped, Table.hover]
                    , thead = Table.simpleThead
                        [ Table.th [] [text "Capture Slug"]
                        , Table.th [] [text "Captured at"]
                        ]
                    , tbody = Table.tbody [] (List.map mkRow session.sessions)
                    }
                ]



