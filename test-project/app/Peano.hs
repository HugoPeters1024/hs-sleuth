module Peano where

data Nat = Z | S Nat

natCata :: r -> (r -> r) -> Nat -> r
natCata zero succ = f
  where f Z = zero
        f (S n) = succ (f n)

{-# RULES
  "natConst1"   natToInt (S Z) = 1
#-}

natToInt :: Nat -> Int
natToInt Z = 0
natToInt (S n) = 1 + natToInt n

peanoSix :: Nat
peanoSix = S Z

six :: Int
six = natToInt peanoSix
