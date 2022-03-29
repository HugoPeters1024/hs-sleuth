module Main where

say :: String -> String
say msg = let ap = "!" in msg ++ ap

main :: IO ()
main = do
    putStrLn $ say "hello"
    putStrLn $ say "world"

