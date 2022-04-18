module Types exposing (..)

import Http
import Html exposing (Html, text)

import Either exposing (Either)

import Generated.HsCore as H

type Loading a = NotRequested
               | Loading (Maybe a)
               | Error Http.Error
               | Ready a

loadFromResult : Result Http.Error a -> Loading a
loadFromResult res = case res of
    Err x -> Error x
    Ok x -> Ready x

liftLoading : Loading a -> (a -> Html msg) -> Html msg
liftLoading load f = case load of
    Ready x -> f x
    Loading (Just x) -> f x
    _       -> text (Debug.toString load)

setLoading : Loading a -> Loading a
setLoading load = case load of
    Ready x -> Loading (Just x)
    _       -> Loading Nothing

type alias Model = 
    { moduleLoading : Loading H.Module
    , selectedTerm : Maybe (Either H.Binder H.ExternalName)
    }
type Msg = MsgGotModule (Result Http.Error H.Module)
         | MsgSelectTerm (Either H.Binder H.ExternalName)
         | MsgLoadModule String Int
