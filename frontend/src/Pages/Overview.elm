module Pages.Overview exposing (view, init)

import Html exposing (..)
import Types exposing (..)
import Loading exposing (Loading)
import Commands

init : Cmd Msg
init = Commands.fetchSessionMeta

view : Model -> Html Msg
view m = Loading.renderLoading "SessionMeta" m.sessionMetaLoading <| \session ->
            div []
                [ h1 [] [text "Overview"]
                , ul [] (List.map renderSessionItem session.sessions)
                ]


renderSessionItem : String -> Html Msg
renderSessionItem slug = li [] [text slug]


