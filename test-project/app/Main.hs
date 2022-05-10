{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import TreeMap
import HalfMatch
import Factorial
import Extra.Hidden

import Data.Char

data Step s c = Done | Yield c s | Skip s

data Stream c = forall s. Stream (s -> Step s c) s String

{-# Rules
   "writeUp/readUp erasion"                     readUp . writeUp    = id  ;
   "writeUp/readUp erasion2"         forall xs. readUp (writeUp xs) = xs  ;
#-}


myMap f = writeUp . mapS f . readUp


{-# NOINLINE readUp #-}
readUp :: [a] -> Stream a
readUp s = Stream next s []
    where
        next (c:tl) = Yield c tl
        next []     = Done


{-# NOINLINE writeUp #-}
writeUp :: Stream a -> [a]
writeUp (Stream next s n) = go s
    where go s = case next s of
                   Done -> []
                   Yield c s -> c : go s
                   Skip s -> go s



mapS :: (a -> b) -> Stream a -> Stream b
mapS f (Stream next s n) = Stream next' s n
    where next' s = case next s of
                      Done -> Done
                      Yield x s' -> Yield (f x) s'
                      Skip s' -> Skip s'

bigOp :: [Int] -> [Int]
bigOp = myMap (+1) . myMap (+1) . myMap (+1)



msg = bigOp [0,1,2,3]


main :: IO ()
main = putStrLn "Look at the core!!"

