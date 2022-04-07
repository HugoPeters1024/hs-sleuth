module MsgTypes exposing (..)

import Http
import Core.Generated.Types exposing (..)

type Msg = MsgFetchPass String Int
         | MsgGotPass (Result Http.Error PassInfo)
         | MsgFetchSrc String
         | MsgGotSrc (Result Http.Error String)
         | MsgFetchMeta
         | MsgGotMeta (Result Http.Error MetaInfo)
         | MsgToggleHiddenBind Int
         | MsgHideAllBinds
         | MsgSelectTerm CoreId
         | MsgRenameTerm Int String
         | MsgToggleViewTypes
         | MsgToggleUniqueName
         | MsgToggleShowSource
