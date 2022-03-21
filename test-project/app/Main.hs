module Main where

import HsComprehension
import Data.Char

{-# ANN oneMore CoreTrace #-}
oneMore :: Int -> Int
oneMore 0 = 1
oneMore n = n+1

main :: IO ()
main = pure ()
