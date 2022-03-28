module Main where

data Lol = Haha | Hihi

oneMore :: Int -> Int
oneMore x = let y = x + 1 in y * y

norm :: Lol -> Lol
norm Hihi = Haha
norm Haha = Haha

main :: IO ()
main = putStrLn "Hello Haskell"
