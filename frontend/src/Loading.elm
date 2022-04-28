module Loading exposing (..)

import Http
import Html exposing (Html, text)

type Loading a = NotRequested
               | Loading (Maybe a)
               | Error Http.Error
               | Ready a

loadFromResult : Result Http.Error a -> Loading a
loadFromResult res = case res of
    Err x -> Error x
    Ok x -> Ready x

setLoading : Loading a -> Loading a
setLoading load = case load of
    Ready x -> Loading (Just x)
    _       -> Loading Nothing


renderLoading : String -> Loading a -> (a -> Html msg) -> Html msg
renderLoading name load f = case load of
    Ready x -> f x
    Loading (Just x) -> f x
    Loading Nothing -> text ("Loading " ++ name)
    Error e -> text ("Error loading " ++ name ++ ": " ++ Debug.toString e)
    NotRequested -> text ("Not yet started loading " ++ name)

