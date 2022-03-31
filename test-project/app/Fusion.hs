{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Char

data Step s c = Done | Yield c s | Skip s

data Stream c = forall s. Stream (s -> Step s c) s String

{-# Rules
   "map forest" forall f. myMap f = writeUp . mapS f . readUp
#-}

{-# Rules
   "writeUp/readUp erasion" writeUp . readUp = id
#-}

{-# Rules
   "writeUp/readUp erasion2" forall xs. writeUp (readUp xs) = xs
#-}


{-# NOINLINE myMap #-}
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs


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
main = print msg

