module Main where

addThreeN :: Num a => [a] -> [a]
addThreeN = map (+1) . map (+1) . map (+1)

main :: IO ()
main = putStrLn "Hello Haskell"
