{-# LANGUAGE TemplateHaskell #-}
module Tree where

import Debug.Trace
import Test.Inspection
import HsComprehension.Plugin (dumpThisModule)

dumpThisModule

data Tree a 
  = Leaf a 
  | Node (Tree a) (Tree a) 
  deriving Show

{-# Rules
    "mapTree/mapTree"   forall f g t.   mapTree f (mapTree g t) = mapTree (f . g) t   ;
#-}

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf x) = Leaf (f (trace "ping" x))
mapTree f (Node lhs rhs) = Node (mapTree f lhs) (mapTree f rhs)

makeImportant :: Tree Int -> Tree String
makeImportant = mapTree (++"!") . mapTree show

makeImportantFused :: Tree Int -> Tree String
makeImportantFused = mapTree (\x -> show x ++ "!")

inspect $ 'makeImportant === 'makeImportantFused

