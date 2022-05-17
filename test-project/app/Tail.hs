module Tail where

count :: [a] -> Int
count = count' 0
    where count' :: Int -> [a] -> Int
          count' n [] = n
          count' n (x:xs) = count' (n+1) xs


