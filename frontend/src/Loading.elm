module Loading exposing (..)

import Http
import Html exposing (Html, text)
import Html.Attributes as Attributes

import Bootstrap.Spinner as Spinner

type Loading a = NotRequested
               | Loading (Maybe a)
               | Error Http.Error
               | Ready a

loadFromResult : Result Http.Error a -> Loading a
loadFromResult res = case res of
    Err x -> (Error x) --Debug.log (Debug.toString x) (Error x)
    Ok x -> Ready x

setLoading : Loading a -> Loading a
setLoading load = case load of
    Ready x -> Loading (Just x)
    _       -> Loading Nothing


debugLoading : String -> Loading a -> (a -> Html msg) -> Html msg
debugLoading name load f = case load of
    Ready x -> f x
    Loading (Just x) -> f x
    Loading Nothing -> text ("Loading " ++ name)
    Error _ -> text ("Error loading " ++ name)
    NotRequested -> text ("Not yet started loading " ++ name)

renderLoadingWith : Html msg -> Loading a -> (a -> Html msg) -> Html msg
renderLoadingWith load_html load f = case load of
    Ready x -> f x
    Loading (Just x) -> f x
    Error _ -> Html.h1 [Attributes.style "color" "red"] [text "x"]
    _ -> load_html

renderLoading : Loading a -> (a -> Html msg) -> Html msg
renderLoading = renderLoadingWith (Spinner.spinner [] [])

map : (a -> b) -> Loading a -> Loading b
map f load = case load of
    Ready x -> Ready (f x)
    Loading (Just x) -> Loading (Just (f x))
    Loading Nothing -> Loading Nothing
    Error e -> Error e
    NotRequested -> NotRequested

withDefault : a -> Loading a -> a
withDefault def load = case load of
    Ready x -> x
    Loading (Just x) -> x
    _  -> def

toMaybe : Loading a -> Maybe a
toMaybe l = case l of
  Ready x -> Just x
  _ -> Nothing
