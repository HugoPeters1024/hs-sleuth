module Quicksort where

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let lesser = filter (<x) xs
        more   = filter (>x) xs
    in quicksort lesser ++ [x] ++ quicksort more
