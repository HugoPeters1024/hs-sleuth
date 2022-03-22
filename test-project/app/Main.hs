module Main where

double :: Num a => a -> a
double x = x * 2

doubleInt :: Int -> Int
doubleInt = double

main :: IO ()
main = pure ()
