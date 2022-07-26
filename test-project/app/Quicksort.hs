module Quicksort where

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
uicksort (x:xs) =
    let lesser = filter (<x) xs
        more   = filter (>x) xs
    in quicksort lesser ++ [x] ++ quicksort more
