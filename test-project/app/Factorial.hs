module Factorial where

fac :: Int -> Int
fac 0 = 0
fac 1 = 1
fac n = n * fac (n-1)

plusOne :: Int -> Int
plusOne n = n + 1
