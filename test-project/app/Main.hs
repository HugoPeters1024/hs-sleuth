{-# LANGUAGE OverloadedStrings #-}
module Main where

import Factorial (fac)

triangular :: Int -> Int
triangular 0 = 0
triangular n = n + triangular (n-1)

main :: IO ()
main = print $ triangular 10000000
