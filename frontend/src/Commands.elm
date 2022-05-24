module Commands exposing (..)

import Types exposing (..)
import Generated.Types exposing (..)
import Loading exposing (..)
import Http
import Generated.Decoders as HE

import HsCore.Trafo.Reconstruct as TR
import Json.Decode


fetchCodePhase : TabId -> Slug -> String -> Int -> Cmd Msg
fetchCodePhase tid slug mod id = 
    Http.get { url = "http://localhost:8080/core/" ++ slug ++ "/" ++ mod ++ "/" ++ String.fromInt id
             , expect = Http.expectJson (MsgCodeMsg tid << CodeMsgGotModule slug) (Json.Decode.map TR.reconModule HE.moduleDecoder)
             }

fetchSessionMeta : Cmd Msg
fetchSessionMeta = Http.get { url = "http://localhost:8080/captures"
                            , expect = Http.expectJson MsgGotCaptures (Json.Decode.list HE.captureDecoder)
                            }
