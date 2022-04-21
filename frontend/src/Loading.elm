module Loading exposing (..)

import Http
import Html exposing (Html, text)

type Loading a = NotRequested
               | Loading (Maybe a)
               | Error Http.Error
               | Ready a

loadOrDebug : Loading a -> (a -> Html msg) -> Html msg
loadOrDebug load f = case load of
    Ready x -> f x
    Loading (Just x) -> f x
    _ -> text (Debug.toString load)

loadFromResult : Result Http.Error a -> Loading a
loadFromResult res = case res of
    Err x -> Error x
    Ok x -> Ready x

liftLoading :  Loading a -> b -> (a -> b) -> b
liftLoading load def f = case load of
    Ready x -> f x
    Loading (Just x) -> f x
    _       -> def

setLoading : Loading a -> Loading a
setLoading load = case load of
    Ready x -> Loading (Just x)
    _       -> Loading Nothing

