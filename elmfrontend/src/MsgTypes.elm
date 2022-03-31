module MsgTypes exposing (..)

import Http

import Core.Generated.Types exposing (..)

type Msg = MsgGotPass (Result Http.Error PassInfo)
         | MsgFetchPass Int
         | MsgToggleHiddenBind String
         | MsgHideAllBinds
         | MsgSelectTerm CoreId
         | MsgRenameTerm String String
