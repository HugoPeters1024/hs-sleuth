module Main where

test :: [Int]
test = map (+1) (map (+1) [1000])

main :: IO ()
main = pure ()
