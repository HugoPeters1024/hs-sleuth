module Factorial where

fac :: Int -> Int
fac 0 = 0
fac 1 = 1
fac n = n * fac (n-1)


halves :: [Int] -> [Int]
halves = map (`div` 2) . filter even

halves_fast :: [Int] -> [Int]
halves_fast [] = []
halves_fast (x:xs) = 
  let 
    tl = halves_fast xs 
  in if even x 
     then (x `div` 2):tl
     else tl
