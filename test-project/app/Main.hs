module Main where

plusOne :: Int -> Int
plusOne x = let 
    y = 
        x 
        + 
        1 
    in y * y

main :: IO ()
main = pure ()
