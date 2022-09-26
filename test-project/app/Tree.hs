module Tree where

import Data.Maybe (fromMaybe)

data Tree a = Leaf a 
            | Node (Tree a) (Tree a) 
            deriving Show

{-# Rules
   "mapTree mapTree"   forall f g.   mapTree f . mapTree g    = mapTree (f. g)   ;
#-}

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf x) = Leaf (f x)
mapTree f (Node lhs rhs) = Node (mapTree f lhs) (mapTree f rhs)

intSqrt :: Int -> Maybe Int
intSqrt n = go n
  where go x
          | x * x > n    = go (x-1)
          | x * x == n   = Just x
          | otherwise    = Nothing


treeSqrt :: Tree Int -> Tree Int
treeSqrt = mapTree (fromMaybe 0) . mapTree intSqrt
