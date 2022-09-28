module UI.FileDropper exposing (..)

import Json.Decode as D
import File exposing (File)
import File.Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

type alias Model =
  { hover : Bool
  , file_selectors : List String
  }

init : List String -> Model
init selectors =
  { hover = False
  , file_selectors = selectors
  }

type Msg 
  = MsgPick
  | MsgDragEnter
  | MsgDragLeave
  | MsgGotFiles File (List File)

update : Msg -> Model -> (Model, Cmd Msg, List File)
update msg model = case msg of
  MsgPick -> (model, File.Select.files model.file_selectors MsgGotFiles, [])
  MsgDragEnter -> ({ model | hover = True}, Cmd.none, [])
  MsgDragLeave -> ({ model | hover = False}, Cmd.none, [])
  MsgGotFiles file files -> ({ model | hover = False }, Cmd.none, file::files)

type alias ViewConfig msg =
  { lift : Msg -> msg
  } 

viewConfig : (Msg -> msg) -> ViewConfig msg
viewConfig f = { lift = f }

hijack : msg -> (msg, Bool)
hijack msg = (msg, True)

hijackOn : String -> D.Decoder msg -> Attribute msg
hijackOn event decoder =
  preventDefaultOn event (D.map hijack decoder)

dropDecoder : D.Decoder Msg
dropDecoder =
  D.at ["dataTransfer","files"] (D.oneOrMore MsgGotFiles File.decoder)

view : Model -> ViewConfig msg -> Html msg
view model opt = Html.map opt.lift <|
  div
    [ style "border" (if model.hover then "6px dashed purple" else "6px dashed #ccc")
    , style "border-radius" "20px"
    , style "width" "90%"
    , style "height" "150px"
    , style "margin" "100px auto"
    , style "padding" "20px"
    , style "display" "flex"
    , style "flex-direction" "column"
    , style "justify-content" "center"
    , style "align-items" "center"
    , hijackOn "dragenter" (D.succeed MsgDragEnter)
    , hijackOn "dragover" (D.succeed MsgDragEnter)
    , hijackOn "dragleave" (D.succeed MsgDragLeave)
    , hijackOn "drop" dropDecoder
    ]
    [ button [ onClick MsgPick, style "padding" "2rem" ] [ text "Load Dump Zip(s)" ]
    ] 

