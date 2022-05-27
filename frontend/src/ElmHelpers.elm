module ElmHelpers exposing (..)

import Set.Any

zip : List a -> List b -> List (a, b)
zip lhs rhs = case (lhs, rhs) of
    (x::xs, y::ys) -> (x,y) :: zip xs ys
    _              -> []

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


removeDuplicatesKey : (a -> comparable) -> List a -> List a
removeDuplicatesKey f = Set.Any.toList << Set.Any.fromList f

removeDuplicates : List comparable -> List comparable
removeDuplicates = removeDuplicatesKey identity

