module Typeclass where

data List a = Emtpy | Cons a (List a)

class PoliteShow a where
  politeShow :: a -> String

instance PoliteShow Int where
  politeShow x = show x ++ " please"

instance PoliteShow a => PoliteShow (List a) where
  politeShow Emtpy = "empty list please"
  politeShow (Cons x xs) = politeShow x ++ " and then " ++ politeShow xs

test :: String
test = 
  let 
      ls :: List Int
      ls = Cons 1 (Cons 2 Emtpy)
  in politeShow ls





