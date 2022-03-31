module Main where

machteld :: [Int] -> [Int]
machteld = map (+1) . map (+1) . map (+1)

main :: IO ()
main = putStrLn "Hello Haskell"
