module Pages.Overview exposing (view, init, update)

import Html exposing (..)
import Html.Events exposing (..)
import HtmlHelpers exposing (..)
import Types exposing (..)
import Loading exposing (Loading)
import Commands

import Set.Any exposing (AnySet)
import Time

import Bootstrap.Table as Table
import Bootstrap.Button as Button

init : Cmd Msg
init = Commands.fetchSessionMeta

update : OverviewMsg -> OverviewTab -> (OverviewTab, Cmd Msg)
update msg tab = case msg of
    OverviewMsgToggleProject project -> ({tab | enabledProjects = Set.Any.toggle project tab.enabledProjects}, Cmd.none)

view : Model -> Html Msg
view m = 
    let
        mkRow project = 
            Table.tr [Table.rowAttr (onClick (MsgOverViewTab (OverviewMsgToggleProject project)))] 
                [ Table.td [] [ checkboxSimple
                                (Set.Any.member project m.overviewTab.enabledProjects) 
                              ]
                , Table.td [] [text project.captureName]
                , Table.td [] [text (renderDateTime m.timezone (Time.millisToPosix project.captureDate))]
                ] 
    in Loading.debugLoading "SessionMeta" m.capturesLoading <| \captures ->
            div []
                [ h1 [] [text "Overview"]
                , Table.table
                    { options = [Table.striped, Table.hover]
                    , thead = Table.simpleThead
                        [ Table.th [] [text "selected"]
                        , Table.th [] [text "Capture Slug"]
                        , Table.th [] [text "Captured at"]
                        ]
                    , tbody = Table.tbody [] (List.map mkRow captures)
                    }
                , Button.button 
                    [ Button.primary
                    , Button.disabled (Set.Any.isEmpty m.overviewTab.enabledProjects)
                    , Button.attrs [onClick MsgOpenCodeTab]
                    ] 
                    [text "Open Tab"]
                ]



