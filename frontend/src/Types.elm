module Types exposing (..)

import Http
import Html exposing (Html, text)

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
    }
type Msg = GotModule (Result Http.Error HsCore.Module)
