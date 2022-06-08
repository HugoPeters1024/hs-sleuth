{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Properties.Utils (
  module Properties.Utils,
  module Test.QuickCheck,
  module Test.QuickCheck.Batch,
  ) where

import Test.QuickCheck
import Test.QuickCheck.Batch
import Text.Show.Functions

import Control.Monad (liftM,liftM5)

import qualified Data.Stream as S

opts = TestOptions {
         no_of_tests     = 200,
         length_of_tests = 0,
         debug_tests = False
       }

eq1 :: (Eq a) => (t -> a) -> (t -> a) -> t -> Property
eq2 :: (Eq a) => (t -> t1 -> a) -> (t -> t1 -> a) -> t -> t1 -> Property
eq3 :: (Eq a) => (t -> t1 -> t2 -> a)
    -> (t -> t1 -> t2 -> a)
    -> t -> t1 -> t2 -> Property
eq4 :: (Eq a) => (t -> t1 -> t2 -> t3 -> a)
    -> (t -> t1 -> t2 -> t3 -> a)
    -> t -> t1 -> t2 -> t3 -> Property
eq5 :: (Eq a) => (t -> t1 -> t2 -> t3 -> t4 -> a)
    -> (t -> t1 -> t2 -> t3 -> t4 -> a)
    -> t -> t1 -> t2 -> t3 -> t4 -> Property
eq6 :: (Eq a) => (t -> t1 -> t2 -> t3 -> t4 -> t5 -> a)
    -> (t -> t1 -> t2 -> t3 -> t4 -> t5 -> a)
    -> t -> t1 -> t2 -> t3 -> t4 -> t5 -> Property
eq7 :: (Eq a) => (t -> t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> a)
    -> (t -> t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> a)
    -> t -> t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> Property
eq8 :: (Eq a) => (t -> t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> a)
    -> (t -> t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> a)
    -> t -> t1 -> t2 -> t3 -> t4 -> t5 -> t6 -> t7 -> Property

eq1 f g = \x               -> property $ f x               == g x
eq2 f g = \x y             -> property $ f x y             == g x y
eq3 f g = \x y z           -> property $ f x y z           == g x y z
eq4 f g = \x y z a         -> property $ f x y z a         == g x y z a
eq5 f g = \x y z a b       -> property $ f x y z a b       == g x y z a b
eq6 f g = \x y z a b c     -> property $ f x y z a b c     == g x y z a b c
eq7 f g = \x y z a b c d   -> property $ f x y z a b c d   == g x y z a b c d
eq8 f g = \x y z a b c d e -> property $ f x y z a b c d e == g x y z a b c d e

eqnotnull1 :: (Eq a1) => ([a] -> a1) -> ([a] -> a1) -> [a] -> Property
eqnotnull2 :: (Eq a1) => (t -> [a] -> a1)
           -> (t -> [a] -> a1) -> t -> [a] -> Property
eqnotnull3 :: (Eq a1) => (t -> t1 -> [a] -> a1)
           -> (t -> t1 -> [a] -> a1) -> t -> t1 -> [a] -> Property

eqnotnull1 f g = \x     -> (not (null x)) ==> eq1 f g x
eqnotnull2 f g = \x y   -> (not (null y)) ==> eq2 f g x y
eqnotnull3 f g = \x y z -> (not (null z)) ==> eq3 f g x y z

eqfinite1 f g = \x     -> forAll arbitrary $ \n -> Prelude.take n (f x)     == Prelude.take n (g x)
eqfinite2 f g = \x y   -> forAll arbitrary $ \n -> Prelude.take n (f x y)   == Prelude.take n (g x y)
eqfinite3 f g = \x y z -> forAll arbitrary $ \n -> Prelude.take n (f x y z) == Prelude.take n (g x y z)


newtype A = A Int deriving (Eq, Show, Arbitrary)
newtype B = B Int deriving (Eq, Show, Arbitrary)
newtype C = C Int deriving (Eq, Show, Arbitrary)
type D = A
type E = B
type F = C
type G = A
type H = B

newtype OrdA = OrdA Int deriving (Eq, Ord, Show, Arbitrary)

newtype N = N Int deriving (Eq, Ord, Num, Show, Arbitrary)
newtype I = I Int deriving (Eq, Ord, Num, Enum, Real, Integral, Show, Arbitrary)

instance Arbitrary Char where
    arbitrary     = elements ([' ', '\n', '\0'] ++ ['a'..'h'])
    coarbitrary c = variant (fromEnum c `rem` 4)

instance Arbitrary Ordering where
    arbitrary      = elements [LT, EQ, GT]
    coarbitrary LT = variant 0
    coarbitrary EQ = variant 1
    coarbitrary GT = variant 2

{-
instance Arbitrary a => Arbitrary (Maybe a) where
    arbitrary            = frequency [ (1, return Nothing)
                                     , (3, liftM Just arbitrary) ]
    coarbitrary Nothing  = variant 0
    coarbitrary (Just a) = variant 1 . coarbitrary a
        -}

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e)
      => Arbitrary (a, b, c, d ,e )
 where
  arbitrary = liftM5 (,,,,) arbitrary arbitrary arbitrary arbitrary arbitrary
  coarbitrary (a, b, c, d, e) =
    coarbitrary a . coarbitrary b . coarbitrary c . coarbitrary d .  coarbitrary e

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e, Arbitrary f)
      => Arbitrary (a, b, c, d, e, f)
 where
  arbitrary = liftM6 (,,,,,) arbitrary arbitrary arbitrary arbitrary arbitrary arbitrary
  coarbitrary (a, b, c, d, e, f) =
    coarbitrary a . coarbitrary b . coarbitrary c . coarbitrary d .  coarbitrary e . coarbitrary f

liftM6  :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m a6 -> m r
liftM6 f m1 m2 m3 m4 m5 m6 = do { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5; x6 <- m6; return (f x1 x2 x3 x4 x5 x6) }

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d, Arbitrary e, Arbitrary f, Arbitrary g)
      => Arbitrary (a, b, c, d, e, f, g)
 where
  arbitrary = liftM7 (,,,,,,) arbitrary arbitrary arbitrary arbitrary arbitrary arbitrary arbitrary
  coarbitrary (a, b, c, d, e, f, g) =
    coarbitrary a . coarbitrary b . coarbitrary c . coarbitrary d .  coarbitrary e . coarbitrary f . coarbitrary g

liftM7  :: (Monad m) => (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> r) -> m a1 -> m a2 -> m a3 -> m a4 -> m a5 -> m a6 -> m a7 -> m r
liftM7 f m1 m2 m3 m4 m5 m6 m7 = do { x1 <- m1; x2 <- m2; x3 <- m3; x4 <- m4; x5 <- m5; x6 <- m6; x7 <- m7 ; return (f x1 x2 x3 x4 x5 x6 x7) }


------------------------------------------------------------------------
-- Arbitrary instance for Stream

{-
instance (Arbitrary a, Arbitrary s) => Arbitrary (S.Step a s)  where
    arbitrary = do x <- arbitrary
                   a <- arbitrary
                   s <- arbitrary
                   return $ case x of
                        LT -> S.Yield a s
                        EQ -> S.Skip s
                        GT -> S.Done
    coarbitrary = error "No coarbitrary for Step a s"
-}

-- existential state type
instance (Arbitrary a) => Arbitrary (S.Stream a)  where
    coarbitrary = error "No coarbitrary for Streams"
    arbitrary = do xs    <- arbitrary :: Gen [a]
                   skips <- arbitrary :: Gen [Bool] -- random Skips
                   return (stream' (zip xs skips))
      where
        -- | Construct an abstract stream from a list, with Steps in it.
        stream' :: [(a,Bool)] -> S.Stream a
        stream' xs0 = S.Stream next (S.L xs0)
          where
            next (S.L [])             = S.Done
            next (S.L ((x,True ):xs)) = S.Yield x (S.L xs)
            next (S.L ((_,False):xs)) = S.Skip    (S.L xs)

instance Show a => Show (S.Stream a) where
  show = show . S.unstream

instance Eq a => Eq (S.Stream a) where
  xs == ys = S.unstream xs == S.unstream ys
