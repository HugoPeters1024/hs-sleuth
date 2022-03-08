module Main where

import HsComprehension
import Data.Char

{-# ANN oneMore CoreTrace #-}
oneMore :: Int -> Int
oneMore 0 = 0
oneMore n = oneMore (n-2)

main :: IO ()
main = putStrLn "hello world"
