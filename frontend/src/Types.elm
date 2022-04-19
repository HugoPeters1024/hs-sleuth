module Types exposing (..)

import Http
import Html exposing (Html, text)

import Either exposing (Either)
import Loading exposing (Loading(..))

import Generated.Types as H
import HsCore.Helpers as H



type SelectedTerm = SelectedBinder { binder : H.Binder
                                   , typeStr : String
                                   }
                  | SelectedExternal { external : H.ExternalName
                                     , typeStr : String
                                     }

selectedTermToInt : SelectedTerm -> Int
selectedTermToInt term = case term of
    SelectedBinder b -> H.binderToInt b.binder
    SelectedExternal e -> H.externalNameToInt e.external

type alias Model = 
    { moduleLoading : Loading H.Module
    , selectedTerm : Maybe SelectedTerm
    }
type Msg = MsgGotModule (Result Http.Error H.Module)
         | MsgSelectTerm SelectedTerm
         | MsgLoadModule String Int
