module Loading exposing (..)

import Http

type Loading a = NotRequested
               | Loading (Maybe a)
               | Error Http.Error
               | Ready a

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

