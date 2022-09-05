module Pages.Overview exposing (view, init, update)

import Html exposing (..)
import Html.Attributes exposing (..)
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
init = Cmd.batch [Commands.fetchCaptures, Commands.fetchSettings]

update : OverviewMsg -> OverviewTab -> (OverviewTab, Cmd Msg)
update msg tab = case msg of
    OverviewMsgStageCapture project -> ({tab | stagedProjects = tab.stagedProjects ++ [project]}, Cmd.none)
    OverViewMsgDeleteCapture capture -> (tab, Commands.deleteCapture capture)
    OverViewMsgCaptureDeleted -> (tab, init)

view : Model -> Html Msg
view m = 
    let
        mkRow project = 
            Table.tr []
                [ Table.td [] 
                    [ Button.button 
                        [ Button.secondary
                        , Button.success
                        , Button.attrs [class "bi bi-arrow-bar-down", onClick (lift (OverviewMsgStageCapture project))]
                        ] []
                    , Button.button
                        [ Button.secondary
                        , Button.danger
                        , Button.attrs [class "bi bi-trash", onClick (lift (OverViewMsgDeleteCapture project))]
                        ] []
                    ]
                , Table.td [] [text project.captureName]
                , Table.td [] [text project.captureGhcVersion]
                , Table.td [] [text (renderDateTime m.timezone (Time.millisToPosix project.captureDate))]
                ] 
    in Loading.debugLoading "SessionMeta" m.capturesLoading <| \captures ->
            div []
                [ h1 [] [text "Overview"]
                , hr [] []
                , span [] [h2 [] [text "Captures"], text " (", Loading.renderLoading m.settingsLoading (\s -> text (s.st_baseDir ++ "/coredump-*")),text ")"]
                , Table.table
                    { options = [Table.striped, Table.hover]
                    , thead = Table.simpleThead
                        [ Table.th [] [text "Stage"]
                        , Table.th [] [text "Capture Slug"]
                        , Table.th [] [text "GHC Version"]
                        , Table.th [] [text "Captured at"]
                        ]
                    , tbody = Table.tbody [] (List.map mkRow captures)
                    }
                , hr [] []
                , h2 [] [text "Staged"]
                , HtmlHelpers.list (List.map (text << .captureName) m.overviewTab.stagedProjects)
                , hr [] []
                , Button.button 
                    [ Button.primary
                    , Button.disabled (List.isEmpty m.overviewTab.stagedProjects)
                    , Button.attrs [onClick MsgOpenCodeTab]
                    ] 
                    [text "Open Tab"]
                ]



