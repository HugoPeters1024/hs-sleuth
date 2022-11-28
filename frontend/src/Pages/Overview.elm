module Pages.Overview exposing (view, init, update)

import Generated.Types exposing (..)
import Generated.Decoders

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import HtmlHelpers exposing (..)
import Types exposing (..)
import ElmHelpers as EH

import Dict exposing (Dict)

import UI.FileDropper as FileDropper

import Time
import Json.Decode

import Bootstrap.Table as Table
import Bootstrap.Button as Button
import Bootstrap.Alert as Alert
import Bootstrap.Popover as Popover

import File
import File.Select exposing (file)
import Task
import File
import Zip
import Zip.Entry
import Dict

lift : OverviewMsg -> Msg
lift = Types.MsgOverViewTab

init : OverviewTab
init = { stagedProjects = []
       , problem = Nothing
       , captures = []
       , filedropper = FileDropper.init ["application/zip"]
       , capturesPopover = Popover.initialState
       , stagedPopover = Popover.initialState
       }

parseFile : Dict String Zip.Entry.Entry -> String -> Json.Decode.Decoder a -> Result String a
parseFile dict file decoder = case Dict.get file dict of
      Nothing -> Err ("Archive does not contain a " ++ file ++ " file")
      Just entry -> case Zip.Entry.toString entry of
        Err _ -> Err ("There was a problem reading the content of" ++ file)
        Ok string_content -> case Json.Decode.decodeString decoder string_content of
          Err _ -> Err ("There was a problem decoding the content of " ++ file ++ " in " ++ file)
          Ok parsed -> Ok parsed

update : OverviewMsg -> OverviewTab -> (OverviewTab, Cmd Msg)
update msg tab = case msg of
    OverviewMsgStageCapture cv -> ({tab | stagedProjects = tab.stagedProjects ++ [cv]}, Cmd.none)
    OverviewMsgUnstageCapture index -> ({tab | stagedProjects = EH.removeAtIndex index tab.stagedProjects}, Cmd.none)
    OverviewMsgReadDump filename content -> case Zip.fromBytes content |> Maybe.map Zip.entries of
      Nothing -> (overviewSetProblem (filename ++ " is not a valid zip archive") tab, Cmd.none)
      Just entries -> 
        let dict = Dict.fromList (EH.annotate Zip.Entry.basename entries)

            capture_parsed = 
              parseFile dict "capture.json" Generated.Decoders.captureDecoder
              -- overwrite the capture slug to the filename
              |> Result.map (\c -> { c | captureName = filename })

        in case capture_parsed of
          Err problem -> (overviewSetProblem problem tab, Cmd.none)
          Ok capture -> 
            let metas_res = capture.captureModules
                        |> List.map Tuple.first
                        |> List.map (\x -> parseFile dict (x ++ "_meta.json") Generated.Decoders.moduleMetaDecoder)
                        |> EH.resultTraverse
                        |> Result.map (EH.zip (List.map Tuple.first capture.captureModules))
                        |> Result.map (Dict.fromList)

            in case metas_res of
              Err problem -> (overviewSetProblem problem tab, Cmd.none)
              Ok metas ->
                let capture_view : CaptureView
                    capture_view = { capture = capture
                                   , module_metas = metas
                                   , files = dict
                                   , filename = filename
                                   }
                in ( {tab | captures = tab.captures ++ [capture_view]} |> overviewRemoveProblem , Cmd.none)
    OverviewMsgDismissProblem _ -> ({tab | problem = Nothing}, Cmd.none)
    OverviewMsgFileDropper filemsg ->
      let (filedropper, cmd, files) = FileDropper.update filemsg tab.filedropper
          cmds = List.map (\file -> Task.perform (lift << OverviewMsgReadDump (File.name file)) (File.toBytes file)) files
      in ( { tab | filedropper = filedropper }
         , Cmd.batch ((Cmd.map (lift << OverviewMsgFileDropper) cmd) :: cmds)
         )
    OverviewMsgStagedPopover state -> ({tab | stagedPopover = state }, Cmd.none)
    OverviewMsgCapturePopover state -> ({tab | capturesPopover = state }, Cmd.none)



helpPopover : Popover.State -> (Popover.State -> msg) -> String -> String -> Html msg
helpPopover m action title content = 
  Popover.config
       ( Button.button
            [ Button.small
            , Button.outlinePrimary
            , Button.attrs <|
                Popover.onClick m action

            ]
            [ span [class "bi bi-question-circle-fill"] []
            ]
        )
        |> Popover.right
        |> Popover.titleH4 [] [ text title ]
        |> Popover.content []
            [ text content ]
        |> Popover.view m


view : Model -> Html Msg
view m = 
  let
      mkCaptureRow cv = 
        Table.tr []
          [ Table.td [] 
              [ Button.button 
                  [ Button.secondary
                  , Button.success
                    , Button.attrs [class "bi bi-arrow-bar-down", onClick (lift (OverviewMsgStageCapture cv))]
                    ] []
                ]
            , Table.td [] [text cv.filename]
            , Table.td [] [text cv.capture.captureGhcVersion]
            , Table.td [] [text (renderDateTime m.timezone (Time.millisToPosix cv.capture.captureDate))]
            ] 

      mkStagedRow (i, cv) =
          Table.tr []
            [ Table.td []
                  [ Button.button
                      [ Button.secondary
                      , Button.warning
                      , Button.attrs [class "bi bi-arrow-bar-left", onClick (lift (OverviewMsgUnstageCapture i))]
                      ] []
                  ]
            , Table.td [] [text cv.capture.captureName]
            ]
    in div []
           [ p [] []
           , h1 [] [text "Overview"]
           , p []
              [ text "This application can be used to explore haskell core snapshots made using the HsComprehension "
              , a [href "https://github.com/HugoPeters1024/hs-comprehension", target "_blank"] [text "plugin."]
              ]
           , p []
              [ text "You can also test it out first by downloading some "
              , a [href "http://core.hugopeters.me/examples", target "_blank"] [text "example captures."]
              ]
           , hr [] []
           , EH.maybeHtml m.overviewTab.problem <| \problem -> 
                Alert.config
                |> Alert.danger
                |> Alert.dismissable (lift << OverviewMsgDismissProblem)
                |> Alert.children [text problem]
                |> Alert.view Alert.shown
           , FileDropper.viewConfig (lift << OverviewMsgFileDropper)
                |> FileDropper.view m.overviewTab.filedropper
           , h2 [] 
              [ text "Captures "
              , helpPopover m.overviewTab.capturesPopover (lift << OverviewMsgCapturePopover) "Capture area" "Capture archives that are loaded in the app are shown here. Before viewing them you have to stage them below"
              ]
           , Table.table
               { options = [Table.striped, Table.hover]
               , thead = Table.simpleThead
                   [ Table.th [] [text "Actions"]
                   , Table.th [] [text "Capture Archive"]
                   , Table.th [] [text "GHC Version"]
                   , Table.th [] [text "Captured at"]
                   ]
               , tbody = Table.tbody [] (List.map mkCaptureRow m.overviewTab.captures)
               }
           , h2 [style "padding-top" "3rem"] 
              [ text "Staged "
              , helpPopover m.overviewTab.stagedPopover (lift << OverviewMsgStagedPopover) "Staging area" "Captures referenced in this list will be opened side by side"
              ]
           , Table.table
              { options = [Table.striped, Table.hover]
              , thead = Table.simpleThead
                  [ Table.th [] [text "Actions"]
                  , Table.th [] [text "Archive"]
                  ]
              , tbody = Table.tbody [] (List.map mkStagedRow (EH.enumerate m.overviewTab.stagedProjects))
              }
           , p [] []
           , Button.button 
               [ Button.primary
               , Button.disabled (List.isEmpty m.overviewTab.stagedProjects)
               , Button.attrs [onClick MsgOpenCodeTab]
               ] 
               [text ("Open Tab With " ++ String.fromInt (List.length m.overviewTab.stagedProjects) ++ " Panel(s)")]
           ]




