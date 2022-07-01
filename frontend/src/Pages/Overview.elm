module Pages.Overview exposing (view, init, update)

import Html exposing (..)
import Html.Events exposing (..)
import HtmlHelpers exposing (..)
import Types exposing (..)
import Loading exposing (Loading)
import Commands
import HsCore.Helpers as H

import Time

import Bootstrap.Table as Table
import Bootstrap.Button as Button

lift : OverviewMsg -> Msg
lift = Types.MsgOverViewTab

init : Cmd Msg
init = Commands.fetchCaptures

update : OverviewMsg -> OverviewTab -> (OverviewTab, Cmd Msg)
update msg tab = case msg of
    OverviewMsgStageProject project -> ({tab | stagedProjects = tab.stagedProjects ++ [project]}, Cmd.none)

view : Model -> Html Msg
view m = 
    let
        mkRow project = 
            Table.tr [Table.rowAttr (onClick (MsgOverViewTab (OverviewMsgStageProject project)))] 
                [ Table.td [] [ Button.button 
                                    [ Button.secondary
                                    ] 
                                    [ text "+" ] 
                              ]
                , Table.td [] [text project.captureName]
                , Table.td [] [text (renderDateTime m.timezone (Time.millisToPosix project.captureDate))]
                ] 
    in Loading.debugLoading "SessionMeta" m.capturesLoading <| \captures ->
            div []
                [ h1 [] [text "Overview"]
                , hr [] []
                , h2 [] [text "Captures"]
                , Table.table
                    { options = [Table.striped, Table.hover]
                    , thead = Table.simpleThead
                        [ Table.th [] [text "Stage"]
                        , Table.th [] [text "Capture Slug"]
                        , Table.th [] [text "Captured at"]
                        ]
                    , tbody = Table.tbody [] (List.map mkRow captures)
                    }
                , hr [] []
                , HtmlHelpers.list (List.map (text << .captureName) m.overviewTab.stagedProjects)
                , h2 [] [text "Staged"]
                , hr [] []
                , Button.button 
                    [ Button.primary
                    , Button.disabled (List.isEmpty m.overviewTab.stagedProjects)
                    , Button.attrs [onClick MsgOpenCodeTab]
                    ] 
                    [text "Open Tab"]
                ]



