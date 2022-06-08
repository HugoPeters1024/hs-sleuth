{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable  #-}

module Strictness.Utils (
  module Strictness.Utils,
  module Test.SmallCheck.Partial
  ) where

import Prelude hiding (null)
import qualified Prelude

import Test.SmallCheck.Partial hiding (PositiveIntegral(N))
import Test.ChasingBottoms
import Data.Generics

-- some types to use when testing at polymorphic values

newtype A = A () deriving (Eq, Show, Serial, Typeable, Data)
newtype B = B () deriving (Eq, Show, Serial, Typeable, Data)
newtype C = C () deriving (Eq, Show, Serial, Typeable, Data)

type D = A
type E = B
type F = C
type G = A
type H = B

newtype OrdA = OrdA Bool deriving (Eq, Ord, Show, Serial, Typeable, Data)

newtype N = N Int deriving (Eq, Ord, Show, Serial, Typeable, Data)
newtype I = I Int deriving (Eq, Ord, Enum, Real, Integral, Show, Serial, Typeable, Data)

-- to catch cases where we rely on associativity or comutativity of
-- (+) or (*)... make them neither! Mwahahaha :-)

instance Num N where
  N x + N y = N (2 * x + y)
  N x * N y = N (2 * x + y + 1)
  fromInteger = N . fromInteger
  negate (N n) = N (negate n)
  abs (N n) = N (abs n)
  signum (N n) = N (signum n)

instance Num I where
  I x + I y = I (2 * x + y)
  I x * I y = I (2 * x + y + 1)
  fromInteger = I . fromInteger
  negate (I i) = I (negate i)
  abs (I i) = I (abs i)
  signum (I i) = I (signum i)


eq1 f g = \x               -> f x               ==! g x
eq2 f g = \x y             -> f x y             ==! g x y
eq3 f g = \x y z           -> f x y z           ==! g x y z
eq4 f g = \x y z a         -> f x y z a         ==! g x y z a
eq5 f g = \x y z a b       -> f x y z a b       ==! g x y z a b
eq6 f g = \x y z a b c     -> f x y z a b c     ==! g x y z a b c
eq7 f g = \x y z a b c d   -> f x y z a b c d   ==! g x y z a b c d
eq8 f g = \x y z a b c d e -> f x y z a b c d e ==! g x y z a b c d e

eqfinite1 f g = \x         -> semanticEq limit (f x)   (g x)
eqfinite2 f g = \x y       -> semanticEq limit (f x y) (g x y)

limit = Tweak { approxDepth = Just 1000, timeOutLimit = Nothing }


refines1 f g = \x               -> f x               >=! g x
refines2 f g = \x y             -> f x y             >=! g x y
refines3 f g = \x y z           -> f x y z           >=! g x y z
refines4 f g = \x y z a         -> f x y z a         >=! g x y z a
refines5 f g = \x y z a b       -> f x y z a b       >=! g x y z a b
refines6 f g = \x y z a b c     -> f x y z a b c     >=! g x y z a b c
refines7 f g = \x y z a b c d   -> f x y z a b c d   >=! g x y z a b c d
refines8 f g = \x y z a b c d e -> f x y z a b c d e >=! g x y z a b c d e

opts = TestOptions 8 10000
