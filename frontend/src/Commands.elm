module Commands exposing (..)

import Types exposing (..)
import Generated.Types exposing (..)
import Loading exposing (..)
import Http
import Generated.Decoders as HE

import HsCore.Trafo.Reconstruct as TR
import Json.Decode


fetchProjectMeta : Slug -> Cmd Msg
fetchProjectMeta slug = 
    Http.get { url = "http://localhost:8080/" ++ slug ++ "/meta"
             , expect = Http.expectJson MsgGotProjectMeta HE.projectMetaDecoder
             }

fetchCodePhase : TabId -> Slug -> String -> Int -> Cmd Msg
fetchCodePhase tid slug mod id = 
    Http.get { url = "http://localhost:8080/" ++ slug ++ "/" ++ mod ++ "/" ++ String.fromInt id
             , expect = Http.expectJson (MsgCodeMsg tid << CodeMsgGotModule slug) (Json.Decode.map TR.reconModule HE.moduleDecoder)
             }

fetchSessionMeta : Cmd Msg
fetchSessionMeta = Http.get { url = "http://localhost:8080/session"
                            , expect = Http.expectJson MsgGotSessionMeta HE.sessionMetaDecoder
                            }
