module TreeMap where

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving Show

{-# Rules
   "mapTree/mapTree"   forall f g. mapTree f . mapTree g = mapTree (f. g)
#-}

{-# Rules
   "mapTree/id"   mapTree id = id
#-}

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf x) = Leaf (f x)
mapTree f (Node lhs rhs) = Node (mapTree f lhs) (mapTree f rhs)

mapTreeId = mapTree id
