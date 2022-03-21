module Main where

import HsComprehension
import Data.Char

{-# ANN fac CoreTrace #-}
fac :: Int -> Int
fac 0 = 0
fac 1 = 1
fac n = n * fac (n-1)

main :: IO ()
main = pure ()
