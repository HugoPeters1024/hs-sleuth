module TreeMap where

data Tree a = Leaf a 
            | Node (Tree a) (Tree a) 
            deriving Show

{-# Rules
   "mapTree mapTree"   forall f g.   mapTree f . mapTree g    = mapTree (f. g)   ;
   "mapTree mapTree2"  forall t f g. mapTree f (mapTree g t)  = mapTree (f. g) t ;
   "mapTree/id"                      mapTree id               = id               ;
#-}

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf x) = Leaf (f x)
mapTree f (Node lhs rhs) = Node (mapTree f lhs) (mapTree f rhs)

mapTreePlus2 :: Tree Int -> Tree Int
mapTreePlus2 = mapTree (+1) . mapTree (+1)

mapTreeId :: Tree a -> Tree a
mapTreeId = mapTree id
