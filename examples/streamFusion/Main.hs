{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Char

data Step s = Done | Yield Char s | Skip s

data Stream = forall s. Stream (s -> Step s) s String

{-# Rules
   "writeUp/readUp erasion" writeUp . readUp = id
#-}

{-# Rules
   "writeUp/readUp erasion2" forall xs. writeUp (readUp xs) = xs
#-}

{-# Rules
   "map forest" forall f. myMap f = writeUp . mapS f . readUp
#-}

{-# NOINLINE myMap #-}
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs


{-# NOINLINE readUp #-}
readUp :: String -> Stream
readUp s = Stream next s []
    where
        next (c:tl) = Yield c tl
        next []     = Done


{-# NOINLINE writeUp #-}
writeUp :: Stream -> String
writeUp (Stream next s n) = go s
    where go s = case next s of
                   Done -> ""
                   Yield c s -> c : go s
                   Skip s -> go s



{-# NOINLINE mapS #-}
mapS :: (Char -> Char) -> Stream -> Stream
mapS f (Stream next s n) = Stream next' s n
    where next' s = case next s of
                      Done -> Done
                      Yield x s' -> Yield (f x) s'
                      Skip s' -> Skip s'

msg = foldl1 (.) (replicate 10000 (myMap toUpper . myMap toLower)) "Hello Wjfkdsafjaldskfjdskjfklsdjflsajflask;jfklads;jfkladsjflkdsajfklsdjfkdsfklsjfsdklfjakl;fjldsk;ajflskadjfkladsfodsklafjlkads;fjlk;adsfj;sadklfrdsfjdskla;fjdghjfdjghudskjgohgiojis ugisrofhjiesfhjisfghifldldldkfjgndlfitkriodlnlgogkrtidjwjajxjcjerjfjfkfkfkfjitorbjdskjfdskjfskljfaskljfklasjfklsdafddsjflkadsjfklsjfsadkdlffkldshfdsdfhjisldgjiifndslisdhbigirgjijfkldsfjssofhjdsihjioshuisgfhdsifhjsidhfdsifjsjdikfjdsikfld"


main :: IO ()
main = putStrLn msg

