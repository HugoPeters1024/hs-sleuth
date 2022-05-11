module Commands exposing (..)

import Types exposing (..)
import Generated.Types exposing (..)
import Loading exposing (..)
import Http
import Generated.Decoders as HE

import HsCore.Trafo.Reconstruct as TR
import Json.Decode


fetchProjectMeta : Cmd Msg
fetchProjectMeta = Http.get { url = "http://localhost:8080/0/meta"
                            , expect = Http.expectJson MsgGotProjectMeta HE.projectMetaDecoder
                            }

fetchCodeModifyPhase : TabId -> (Int -> Int) -> Module -> Cmd Msg
fetchCodeModifyPhase tid f mod = 
    let n = Basics.clamp 0 19 (f mod.modulePhaseId)
    in if n /= mod.modulePhaseId
       then fetchCodePhase tid mod.moduleName.getModuleName n
       else Cmd.none


fetchCodePhase : TabId -> String -> Int -> Cmd Msg
fetchCodePhase tid mod id = Http.get { url = "http://localhost:8080/0/" ++ mod ++ "/" ++ String.fromInt id
                             , expect = Http.expectJson (MsgCodeMsg tid << CodeMsgGotModule) (Json.Decode.map TR.reconModule HE.moduleDecoder)
                             }

fetchSessionMeta : Cmd Msg
fetchSessionMeta = Http.get { url = "http://localhost:8080/session"
                            , expect = Http.expectJson MsgGotSessionMeta HE.sessionMetaDecoder
                            }
