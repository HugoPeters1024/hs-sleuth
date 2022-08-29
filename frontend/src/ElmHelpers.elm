module ElmHelpers exposing (..)

import Set.Any
import Set exposing (Set)
import Html exposing (h1)

popLast : List a -> Maybe (List a, a)
popLast xs = 
  let n = List.length xs - 1
  in Maybe.map (\x -> (List.take n xs, x)) (List.head (List.drop n xs))

zip : List a -> List b -> List (a, b)
zip lhs rhs = case (lhs, rhs) of
    (x::xs, y::ys) -> (x,y) :: zip xs ys
    _              -> []

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
    []    -> []

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
