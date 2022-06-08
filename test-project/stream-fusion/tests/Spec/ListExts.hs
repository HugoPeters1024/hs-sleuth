-- Spececifications of things in Data.List but not in the H98 List module


module Spec.ListExts (
  foldl',
  foldl1',
  
  intercalate,

  isInfixOf,
  ) where

import Prelude (Int, Integer, Integral, Num(..), Eq(..), Ord(..), Ordering(..),
                Bool(..), (&&), (||), not, Maybe(..), String, 
                (.), error, seq, otherwise, flip)
import Spec.List

foldl'            :: (a -> b -> a) -> a -> [b] -> a
foldl' f z []     =  z
foldl' f z (x:xs) =  let z' = f z x in z' `seq` foldl f z' xs


foldl1'           :: (a -> a -> a) -> [a] -> a
foldl1' f (x:xs)  =  foldl' f x xs
foldl1' _ []      =  error "Prelude.foldl1: empty list"


isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)


intercalate :: [a] -> [[a]] -> [a]
intercalate xs xss = concat (intersperse xs xss)
