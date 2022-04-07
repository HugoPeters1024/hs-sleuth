{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
module Palin where

data Palin xs where
    Empty :: Palin '[]
    Single :: a -> Palin '[a]
    Cons :: a -> Palin xs -> a -> Palin (a ': xs)

{-# RULES
   "flipPalin/flipPalin"  flipPalin . flipPalin = id
#-}

{-# RULES
   "flipPalin/flipPalin2" forall p. flipPalin (flipPalin p) = p
#-}

flipPalin :: Palin xs -> Palin xs
flipPalin Empty = Empty
flipPalin (Single x) = Single x
flipPalin (Cons lhs tl rhs) = Cons rhs (flipPalin tl) lhs

doubleFlipPalin :: Palin xs -> Palin xs
doubleFlipPalin = flipPalin . flipPalin



