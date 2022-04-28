module HalfMatch where

data Pair a b = Pair a b

test :: Pair Int String -> String
test (Pair 0 s) = s
test (Pair n _) = "not 0"
