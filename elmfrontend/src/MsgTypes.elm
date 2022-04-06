module MsgTypes exposing (..)

import Http
import Core.Generated.Types exposing (..)

type Msg = MsgFetchPass Int
         | MsgGotPass (Result Http.Error PassInfo)
         | MsgFetchSrc String
         | MsgGotSrc (Result Http.Error String)
         | MsgToggleHiddenBind Int
         | MsgHideAllBinds
         | MsgSelectTerm CoreId
         | MsgRenameTerm Int String
         | MsgToggleViewTypes
         | MsgToggleUniqueName
         | MsgToggleShowSource
