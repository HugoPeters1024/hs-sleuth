module Commands exposing (..)

import Types exposing (..)
import Generated.Types exposing (..)
import Loading exposing (..)
import Http
import Generated.Decoders as HE

import HsCore.Trafo.Reconstruct as TR
import Json.Decode


defRequest r =
    { method = "GET"
    , headers = []
    , url = r.url
    , body = Http.emptyBody
    , expect = r.expect
    , timeout = Nothing
    , tracker = Nothing
    }


fetchModuleWithSrc : TabId -> SlotId -> Slug -> String -> Cmd Msg
fetchModuleWithSrc tid slot slug mod = Cmd.batch [fetchModule tid slot slug mod, fetchSrc tid slot slug mod]

fetchModule : TabId -> SlotId -> Slug -> String -> Cmd Msg
fetchModule tid slot slug mod = 
  let def = defRequest { url = "http://127.0.0.1:8080/module/" ++ slug ++ "/" ++ mod
                       , expect = Http.expectJson (MsgCodeMsg tid << CodeMsgGotModule slot) (Json.Decode.map TR.reconModule HE.moduleDecoder)
                       }
  in Http.request { def | tracker = Just (String.fromInt slot) }

fetchSrc : TabId -> SlotId -> Slug -> String -> Cmd Msg
fetchSrc tid slot slug mod = 
  let def = defRequest { url = "http://127.0.0.1:8080/src/" ++ slug ++ "/" ++ mod
                       , expect = Http.expectString (MsgCodeMsg tid << CodeMsgGotSrc slot)
                       }
  in Http.request { def | tracker = Nothing }

fetchSettings : Cmd Msg
fetchSettings = Http.get { url = "http://127.0.0.1:8080/settings"
                         , expect = Http.expectJson MsgGotSettings HE.serverSettingsDecoder
                         }

fetchCaptures : Cmd Msg
fetchCaptures = Http.get { url = "http://127.0.0.1:8080/captures"
                         , expect = Http.expectJson MsgGotCaptures (Json.Decode.list HE.captureDecoder)
                         }

deleteCapture : Capture -> Cmd Msg
deleteCapture capture = Http.get { url = "http://127.0.0.1:8080/capture_delete/" ++ capture.captureName
                                 , expect = Http.expectWhatever (\_ -> MsgOverViewTab OverViewMsgCaptureDeleted)
                                 }
