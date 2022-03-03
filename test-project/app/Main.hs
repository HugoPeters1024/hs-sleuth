module Main where

import HsComprehension
import Data.Char


{-# ANN joe CoreTrace #-}
joe :: Int
joe = length $ map (+1) [1,2,3,4,5]

main :: IO ()
main = putStrLn "hi"
