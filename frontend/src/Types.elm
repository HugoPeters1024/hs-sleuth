module Types exposing (..)

import Http
import Html exposing (Html, text)

import Either exposing (Either)

import Generated.HsCore as HsCore

type Loading a = NotRequested
               | Loading
               | Error Http.Error
               | Ready a

loadFromResult : Result Http.Error a -> Loading a
loadFromResult res = case res of
    Err x -> Error x
    Ok x -> Ready x

liftLoading : (a -> Html msg) -> Loading a -> Html msg
liftLoading f load = case load of
    Ready x -> f x
    _       -> text (Debug.toString load)

type alias Model = 
    { moduleLoading : Loading HsCore.Module
    , selectedTerm : Maybe (Either HsCore.Binder HsCore.ExternalName)
    }
type Msg = MsgGotModule (Result Http.Error HsCore.Module)
         | MsgSelectTerm (Either HsCore.Binder HsCore.ExternalName)
