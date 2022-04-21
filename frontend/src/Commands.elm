module Commands exposing (..)

import Types exposing (..)
import Generated.Types exposing (..)
import Loading exposing (..)
import Http
import Generated.Decoders as HE


fetchProjectMeta : Cmd Msg
fetchProjectMeta = Http.get { url = "http://localhost:8080/meta"
                            , expect = Http.expectJson MsgGotProjectMeta HE.projectMetaDecoder
                            }

fetchModifyPhase : (Int -> Int) -> Module -> Cmd Msg
fetchModifyPhase f mod = 
    let n = Basics.clamp 0 19 (f mod.modulePhaseId)
    in if n /= mod.modulePhaseId
       then fetchPhase mod.moduleName.getModuleName n
       else Cmd.none


fetchPhase : String -> Int -> Cmd Msg
fetchPhase mod id = Http.get { url = "http://localhost:8080/" ++ mod ++ "/" ++ String.fromInt id
                            , expect = Http.expectJson MsgGotModule HE.moduleDecoder
                            }
