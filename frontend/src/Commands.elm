module Commands exposing (..)

import Types exposing (..)
import Generated.Types exposing (..)
import Loading exposing (..)
import Http
import Generated.Decoders as HE

import HsCore.Trafo.Reconstruct as TR
import Json.Decode


fetchModule : TabId -> SlotId -> Slug -> String -> Cmd Msg
fetchModule tid slot slug mod = 
    Http.get { url = "http://localhost:8080/module/" ++ slug ++ "/" ++ mod
             , expect = Http.expectJson (MsgCodeMsg tid << CodeMsgGotModule slot) (Json.Decode.map TR.reconModule HE.moduleDecoder)
             }

fetchSettings : Cmd Msg
fetchSettings = Http.get { url = "http://localhost:8080/settings"
                         , expect = Http.expectJson MsgGotSettings HE.serverSettingsDecoder
                         }

fetchCaptures : Cmd Msg
fetchCaptures = Http.get { url = "http://localhost:8080/captures"
                         , expect = Http.expectJson MsgGotCaptures (Json.Decode.list HE.captureDecoder)
                         }

deleteCapture : Capture -> Cmd Msg
deleteCapture capture = Http.get { url = "http://localhost:8080/capture_delete/" ++ capture.captureName
                                 , expect = Http.expectWhatever (\_ -> MsgOverViewTab OverViewMsgCaptureDeleted)
                                 }
