module Main where

import HsComprehension
import Data.Char


{-# ANN joe CoreTrace #-}
joe :: [Int] -> [Char]
joe = map $ chr . (+1)




main :: IO ()
main = putStrLn "hi"
