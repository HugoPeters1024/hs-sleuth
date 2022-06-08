module Factorial where

fac :: Int -> Int
fac 0 = 0
fac 1 = 1
fac n = n * fac (n-1)

fac2 :: Int -> Int
fac2 n = if n == 0 then 0 else if n == 1 then 1 else n * fac2 (n-1)

plusOne :: Int -> Int
plusOne n = n + 1

tupleFirst :: a -> b -> a
tupleFirst x y = x

tupleSecond :: a -> b -> b
tupleSecond x y = y
