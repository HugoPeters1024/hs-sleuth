module Main where

addThreeN :: [Int] -> [Int]
addThreeN = map (+1) . map (+1) . map (+1)

main :: IO ()
main = putStrLn "Hello Haskell"
