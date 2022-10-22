module ElmHelpers exposing (..)

import Http
import Set.Any
import Set exposing (Set)
import Html exposing (text, Html)
import Set.Any exposing (AnySet)

boolToString : Bool -> String
boolToString b = if b then "yes" else "no"

maybeTraverse : List (Maybe a) -> Maybe (List a)
maybeTraverse list = case list of
  [] -> Just []
  x::xs -> Maybe.map2 (::) x (maybeTraverse xs)

resultTraverse : List (Result x a) -> Result x (List a)
resultTraverse list = case list of
  [] -> Ok []
  x::xs -> Result.map2 (::) x (resultTraverse xs)


popLast : List a -> Maybe (List a, a)
popLast xs = 
  let n = List.length xs - 1
  in Maybe.map (\x -> (List.take n xs, x)) (List.head (List.drop n xs))

removeAtIndex : Int -> List a -> List a
removeAtIndex n list = case (n, list) of
  (0, _::xs) -> xs
  (0, []) -> list
  (_, []) -> list
  (_, x::xs) -> x :: removeAtIndex (n - 1) xs

zip : List a -> List b -> List (a, b)
zip lhs rhs = case (lhs, rhs) of
    (x::xs, y::ys) -> (x,y) :: zip xs ys
    _              -> []

dropWhile : (a -> Bool) -> List a -> List a
dropWhile pred list = case list of
  x::xs -> if pred x then dropWhile pred xs else list
  []    -> []


range : Int -> List Int
range = 
    let go : Int -> List Int
        go num = case num of
            0 -> [0]
            n -> n::(go (n - 1))
    in List.reverse << go


enumerate : List a -> List (Int, a)
enumerate xs = zip (range (List.length xs)) xs

allSame : List comparable -> Bool
allSame xxs = case xxs of
  [] -> True
  (x::xs) -> List.all (\y -> x == y) xs



zipWith : (a -> b -> c) -> List a -> List b -> List c
zipWith f lhs rhs = List.map (\(x, y) -> f x y) <| zip lhs rhs

annotate : (a -> b) -> List a -> List (b, a)
annotate f xs = zip (List.map f xs) xs

find : (v -> Bool) -> List v -> Maybe v
find pred list = case list of
    [] -> Nothing
    x::xs -> if pred x then Just x else find pred xs

lookup : List (k, v) -> k -> Maybe v
lookup xs k = case xs of
    [] -> Nothing
    (kk,v)::tl -> if k == kk then Just v else lookup tl k

mapMaybe : (a -> Maybe b) -> List a -> List b
mapMaybe f list = case list of
    x::xs -> case f x of
        Just y -> y::mapMaybe f xs
        Nothing -> mapMaybe f xs
    [] -> []

mapResult : (a -> Result x b) -> List a -> List b
mapResult f list = case list of
    x::xs -> case f x of
        Ok y -> y::mapResult f xs
        Err _ ->  mapResult f xs
    [] -> []
   

indexList : Int -> List a -> Maybe a
indexList n xs = List.head (List.drop n xs)


addIf : Bool -> a -> List a -> List a
addIf b a xs = if b then a::xs else xs


removeDuplicatesKey : (a -> comparable) -> List a -> List a
removeDuplicatesKey f = Set.Any.toList << Set.Any.fromList f

removeDuplicates : List comparable -> List comparable
removeDuplicates = removeDuplicatesKey identity

toggleSet : comparable -> Set comparable -> Set comparable
toggleSet el set = if Set.member el set then Set.remove el set else Set.insert el set

setCombine : List (Set comparable) -> Set comparable
setCombine = List.foldl Set.union Set.empty

anySetIntersectMany : (x -> comparable) -> List (AnySet comparable x) -> AnySet comparable x
anySetIntersectMany f list = case list of
  x::[] -> x
  [] -> Set.Any.empty f
  x::xs -> Set.Any.intersect x (anySetIntersectMany f xs)

setInsertMany : List comparable -> Set comparable -> Set comparable
setInsertMany items set = List.foldl Set.insert set items

setRemoveMany : List comparable -> Set comparable -> Set comparable
setRemoveMany items set = List.foldl Set.remove set items

progressBytes : Http.Progress -> Int
progressBytes progress = case progress of
  Http.Sending x -> x.sent
  Http.Receiving x -> x.received

maybeHtml : Maybe a -> (a -> Html msg) -> Html msg
maybeHtml m f = case m of
  Nothing -> text ""
  Just x -> f x
