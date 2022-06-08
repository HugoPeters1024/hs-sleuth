{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeOperators             #-}


-- |
-- Module      : Data.Stream
-- Copyright   : (c) Duncan Coutts 2007
--               (c) Don Stewart   2007-2013
-- License     : BSD-style
-- Maintainer  : dons00@gmail.com
-- Stability   : experimental
-- Portability : portable, requires cpp
-- Tested with : GHC 6.6
-- 
-- Stream fusion for sequences. Described in: 
--
-- *    /Stream Fusion: From Lists to Streams to Nothing at All/, by
--      Duncan Coutts, Roman Leshchinskiy and Don Stwwart, ICFP 2007.
--      <http://www.cse.unsw.edu.au/~dons/papers/CLS07.html>
--
-- *    /Rewriting Haskell Strings/, by Duncan Coutts, Don Stewart and
--      Roman Leshchinskiy, Practical Aspects of Declarative Languages
--      8th International Symposium, PADL 2007, 2007.
--      <http://www.cse.unsw.edu.au/~dons/papers/CSL06.html>
--
-- See the source for the complete story:
--
-- * <http://www.cse.unsw.edu.au/~dons/code/streams/list/Data/Stream.hs>
--

module Data.Stream (

#ifndef __HADDOCK__

    -- * The stream data type
    Stream(Stream),
    Step(..),

    -- * Conversions with lists
    stream,                 -- :: [a] -> Stream a
    unstream,               -- :: Stream a -> [a]

    -- internal grunge
    L(L),   -- hmm, does this affect whether these get removed?

    -- * Basic stream functions
    append,                 -- :: Stream a -> Stream a -> Stream a
    append1,                 -- :: Stream a -> [a] -> [a]
    cons,                   -- :: a -> Stream a -> Stream a
    snoc,                   -- :: Stream a -> a -> Stream a
    head,                   -- :: Stream a -> a
    last,                   -- :: Stream a -> a
    tail,                   -- :: Stream a -> Stream a
    init,                   -- :: Stream a -> Stream a
    null,                   -- :: Stream a -> Bool
    length,                 -- :: Stream a -> Int

    -- * Stream transformations
    map,                    -- :: (a -> b) -> Stream a -> Stream b
    --  reverse,                -- :: Stream a -> Stream a
    intersperse,            -- :: a -> Stream a -> Stream a
    --  intercalate,          -- :: Stream a -> Stream (Stream a) -> Stream a
    --  transpose,              -- :: Stream (Stream a) -> Stream (Stream a)

    -- * Reducing streams (folds)
    foldl,                  -- :: (b -> a -> b) -> b -> Stream a -> b
    foldl',                 -- :: (b -> a -> b) -> b -> Stream a -> b
    foldl1,                 -- :: (a -> a -> a) -> Stream a -> a
    foldl1',                -- :: (a -> a -> a) -> Stream a -> a
    foldr,                  -- :: (a -> b -> b) -> b -> Stream a -> b
    foldr1,                 -- :: (a -> a -> a) -> Stream a -> a

    -- ** Special folds
    concat,                 -- :: Stream [a] -> [a]
    concatMap,              -- :: (a -> Stream b) -> Stream a -> Stream b
    and,                    -- :: Stream Bool -> Bool
    or,                     -- :: Stream Bool -> Bool
    any,                    -- :: (a -> Bool) -> Stream a -> Bool
    all,                    -- :: (a -> Bool) -> Stream a -> Bool
    sum,                    -- :: Num a => Stream a -> a
    product,                -- :: Num a => Stream a -> a
    maximum,                -- :: Ord a => Stream a -> a
    minimum,                -- :: Ord a => Stream a -> a
    strictMaximum,          -- :: Ord a => Stream a -> a
    strictMinimum,          -- :: Ord a => Stream a -> a

    -- * Building lists
    -- ** Scans
    scanl,                  -- :: (a -> b -> a) -> a -> Stream b -> Stream a
    scanl1,                 -- :: (a -> a -> a) -> Stream a -> Stream a
{-
    scanr,                  -- :: (a -> b -> b) -> b -> Stream a -> Stream b
    scanr1,                 -- :: (a -> a -> a) -> Stream a -> Stream a
-}

{-
    -- ** Accumulating maps
    mapAccumL,              -- :: (acc -> x -> (acc, y)) -> acc -> Stream x -> (acc, Stream y)
    mapAccumR,              -- :: (acc -> x -> (acc, y)) -> acc -> Stream x -> (acc, Stream y)
-}

    -- ** Infinite streams
    iterate,                -- :: (a -> a) -> a -> Stream a
    repeat,                 -- :: a -> Stream a
    replicate,              -- :: Int -> a -> Stream a
    cycle,                  -- :: Stream a -> Stream a

    -- ** Unfolding
    unfoldr,                -- :: (b -> Maybe (a, b)) -> b -> Stream a

    -- * Substreams
    -- ** Extracting substreams
    take,                   -- :: Int -> Stream a -> Stream a
    drop,                   -- :: Int -> Stream a -> Stream a
    splitAt,                -- :: Int -> Stream a -> ([a], [a])
    takeWhile,              -- :: (a -> Bool) -> Stream a -> Stream a
    dropWhile,              -- :: (a -> Bool) -> Stream a -> Stream a
{-
    span,                   -- :: (a -> Bool) -> Stream a -> (Stream a, Stream a)
    break,                  -- :: (a -> Bool) -> Stream a -> (Stream a, Stream a)
    group,                  -- :: Eq a => Stream a -> Stream (Stream a)
    inits,                  -- :: Stream a -> Stream (Stream a)
    tails,                  -- :: Stream a -> Stream (Stream a)
-}

    -- * Predicates
    isPrefixOf,             -- :: Eq a => Stream a -> Stream a -> Bool
{-
    isSuffixOf,             -- :: Eq a => Stream a -> Stream a -> Bool
    isInfixOf,              -- :: Eq a => Stream a -> Stream a -> Bool
-}

    -- * Searching streams
    -- ** Searching by equality
    elem,                   -- :: Eq a => a -> Stream a -> Bool
    lookup,                 -- :: Eq a => a -> Stream (a, b) -> Maybe b

    -- ** Searching with a predicate
    find,                   -- :: (a -> Bool) -> Stream a -> Maybe a
    filter,                 -- :: (a -> Bool) -> Stream a -> Stream a
--  partition,              -- :: (a -> Bool) -> Stream a -> ([a], [a])

    -- * Indexing streams
    index,                  --  :: Stream a -> Int -> a
    findIndex,              -- :: (a -> Bool) -> Stream a -> Maybe Int
    elemIndex,              -- :: Eq a => a -> Stream a -> Maybe Int
    elemIndices,            -- :: Eq a => a -> Stream a -> Stream Int
    findIndices,            -- :: (a -> Bool) -> Stream a -> Stream Int

    -- * Zipping and unzipping streams
    zip,                    -- :: Stream a -> Stream b -> Stream (a, b)
    zip3,                   -- :: Stream a -> Stream b -> Stream c -> Stream (a, b, c)
    zip4,
    zipWith,                -- :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
    zipWith3,               -- :: (a -> b -> c -> d) -> Stream a -> Stream b -> Stream c -> Stream d
    zipWith4,

{-
    zip4, zip5, zip6, zip7,

    zipWith4, zipWith5, zipWith6, zipWith7,
-}
    unzip,                  -- :: Stream (a, b) -> (Stream a, Stream b)
{-
    unzip3,                 -- :: Stream (a, b, c) -> (Stream a, Stream b, Stream c)
    unzip4, unzip5, unzip6, unzip7,
-}

    -- * Special streams
    -- ** Functions on strings
{-
    lines,                  -- :: Stream Char -> Stream [Char]
    unlines,                -- :: Stream (Stream Char) -> Stream Char
    words,                  -- :: Stream Char -> Stream (Stream Char)
    unwords,                -- :: Stream (Stream Char) -> Stream Char
-}

{-
    -- ** \"Set\" operations
    nub,                    -- :: Eq a => Stream a -> Stream a
    delete,                 -- :: Eq a => a -> Stream a -> Stream a
    (\\),                   -- :: Eq a => Stream a -> Stream a -> Stream a
    union,                  -- :: Eq a => Stream a -> Stream a -> Stream a
    intersect,              -- :: Eq a => Stream a -> Stream a -> Stream a
-}

{-
    -- ** Ordered streams 
    sort,                   -- :: Ord a => Stream a -> Stream a
    insert,                 -- :: Ord a => a -> Stream a -> Stream a
-}

{-
    -- * Generalized functions
    -- ** The \"By\" operations
    -- *** User-supplied equality (replacing an Eq context)
    nubBy,                  -- :: (a -> a -> Bool) -> Stream a -> Stream a
    deleteBy,               -- :: (a -> a -> Bool) -> a -> Stream a -> Stream a
    deleteFirstsBy,         -- :: (a -> a -> Bool) -> Stream a -> Stream a -> Stream a
    unionBy,                -- :: (a -> a -> Bool) -> Stream a -> Stream a -> Stream a
    intersectBy,            -- :: (a -> a -> Bool) -> Stream a -> Stream a -> Stream a
    groupBy,                -- :: (a -> a -> Bool) -> Stream a -> Stream (Stream a)
-}

    -- *** User-supplied comparison (replacing an Ord context)
    insertBy,               -- :: (a -> a -> Ordering) -> a -> Stream a -> Stream a
{-
    sortBy,                 -- :: (a -> a -> Ordering) -> Stream a -> Stream a
-}
    maximumBy,              -- :: (a -> a -> Ordering) -> Stream a -> a
    minimumBy,              -- :: (a -> a -> Ordering) -> Stream a -> a

    -- * The \"generic\" operations
    genericLength,          -- :: Num i => Stream b -> i
    genericTake,            -- :: Integral i => i -> Stream a -> Stream a
    genericDrop,            -- :: Integral i => i -> Stream a -> Stream a
    genericIndex,           -- :: Integral a => Stream b -> a -> b
    genericSplitAt,         -- :: Integral i => i -> Stream a -> ([a], [a])

    -- * Enum
    enumFromToInt,          -- :: Int -> Int -> Stream Int
    enumFromToChar,         -- :: Char -> Char -> Stream Char
    enumDeltaInteger,       -- :: Integer -> Integer -> Stream Integer

    -- * Monad
    foldM,                  -- :: Monad m => (b -> a -> m b) -> b -> Stream a -> m b
    foldM_,                 -- :: Monad m => (b -> a -> m b) -> b -> Stream a -> m ()

    -- * List comprehension desugaring
    return,                 -- :: a -> Stream a
    guard,                  -- :: Bool -> Stream a -> Stream a
    bind,                   -- :: (a -> Bool) -> (a -> [b]) -> [a] -> [b]
    mapFilter,              -- :: (a -> Bool) -> (a ->  b)  -> [a] -> [b]
    declare                 -- :: (a -> Stream b) -> a -> Stream b
#endif

  ) where

#ifndef __HADDOCK__

#ifndef EXTERNAL_PACKAGE

import {-# SOURCE #-} GHC.Err (error)
import {-# SOURCE #-} GHC.Num (Num(..),Integer)
import {-# SOURCE #-} GHC.Real (Integral(..))

import GHC.Base (Int, Char, Eq(..), Ord(..), Functor(..), Bool(..), (&&),
                 Ordering(..),
                 (||),(&&), ($),
                 seq, otherwise, ord, chr,
                 Monad((>>=), (>>)),            -- why >> ? we're not using it
                 -- for error messages:
                 String, (++))
import qualified GHC.Base as Monad (Monad(return))

import Data.Tuple  ()

#else

import Prelude (
    error,
    Num(..),
    Integral(..),
    Integer,
    Int, Char, Eq(..), Ord(..), Functor(..), Ordering(..), Bool(..),
    (&&), (||), ($),
    seq, otherwise,
    Monad((>>=)),
    -- for error messages:
    String, (++))
import qualified Prelude as Monad (Monad(return))

import Data.Char (ord,chr)

#endif

import qualified Data.Maybe (Maybe(..))


------------------------------------------------------------------------
-- The stream data type

-- | A stream.
--
-- It is important that we never construct a bottom stream, because the
-- fusion rule is not true for bottom streams.
--
-- > (replicate 1 True) ++ (tail undefined)
--
-- The suspicion is that under fusion the append will force the bottom.
--
data Stream a = forall s. Unlifted s =>
                          Stream !(s -> Step a s)  -- a stepper function
                                 !s                -- an initial state

-- | A stream step.
--
--   A step either ends a stream, skips a value, or yields a value
--
data Step a s = Yield a !s
              | Skip    !s
              | Done

instance Functor Stream where fmap = map


-- | A class of strict unlifted types. The Unlifted constraint in the
-- Stream type above enforces a separation between user's types and the
-- types used in stream states.
--
class Unlifted a where

  -- | This expose function needs to be called in folds/loops that consume
  -- streams to expose the structure of the stream state to the simplifier
  -- In particular, to SpecConstr.
  --
  expose :: a -> b -> b
  expose = seq

  -- | This makes GHC's optimiser happier; it sometimes produces really bad
  -- code for single-method dictionaries
  --
  unlifted_dummy :: a
  unlifted_dummy = error "unlifted_dummy"

--
-- | Unlifted versions of () and Bool for use in Stream states.
--
data None   = None
instance Unlifted None

-- | A useful unlifted type
data Switch = S1 | S2
instance Unlifted Switch

-- | Unlifted pairs, Maybe and Either
--
data a :!: b = !a :!: !b
instance (Unlifted a, Unlifted b) => Unlifted (a :!: b) where
  expose (a :!: b) s = expose a (expose b s)
  {-# INLINE expose #-}

-- | Unlifted Maybes
data Maybe a = Nothing | Just !a
instance Unlifted a => Unlifted (Maybe a) where
  expose (Just a) s = expose a s
  expose  Nothing s =          s
  {-# INLINE expose #-}

-- | Unlifted sums
data Either a b = Left !a | Right !b
instance (Unlifted a, Unlifted b) => Unlifted (Either a b) where
  expose (Left  a) s = expose a s
  expose (Right b) s = expose b s
  {-# INLINE expose #-}

-- | Some stream functions (notably concatMap) need to use a stream as a state
--
instance Unlifted (Stream a) where
  expose (Stream next s0) s = seq next (seq s0 s)
  {-# INLINE expose #-}

-- | Boxes for user's state. This is the gateway for user's types into unlifted
-- stream states. The L is always safe since it's lifted/lazy, exposing/seqing
-- it does nothing.
-- S is unlifted and so is only suitable for users states that we know we can
-- be strict in. This requires attention and auditing. 
--
data    L a = L a  -- lazy / lifted
newtype S a = S a  -- strict / unlifted

instance Unlifted (L a) where
  expose (L _) s = s
  {-# INLINE expose #-}

instance Unlifted (S a) where
  expose (S a) s = seq a s
  {-# INLINE expose #-}

--
-- coding conventions;
--
--  * we tag local loops with their wrapper's name, so they're easier to
--      spot in Core output
--

-- ---------------------------------------------------------------------
-- List/Stream conversion

-- | Construct an abstract stream from a list.
stream :: [a] -> Stream a
stream xs0 = Stream next (L xs0)
  where
    {-# INLINE next #-}
    next (L [])     = Done
    next (L (x:xs)) = Yield x (L xs)
{-# INLINE [0] stream #-}

-- | Flatten a stream back into a list.
unstream :: Stream a -> [a]
unstream (Stream next s0) = unfold_unstream s0
  where
    unfold_unstream !s = case next s of
      Done       -> []
      Skip    s' -> expose s' $     unfold_unstream s'
      Yield x s' -> expose s' $ x : unfold_unstream s'
{-# INLINE [0] unstream #-}

--
-- /The/ stream fusion rule
--

{-# RULES
"STREAM stream/unstream fusion" forall s.
    stream (unstream s) = s
  #-}

-- ---------------------------------------------------------------------
-- Basic stream functions

-- (++)
append :: Stream a -> Stream a -> Stream a
append (Stream next0 s01) (Stream next1 s02) = Stream next (Left s01)
  where
    {-# INLINE next #-}
    next (Left s1)  = case next0 s1 of
                          Done        -> Skip    (Right s02)
                          Skip s1'    -> Skip    (Left s1')
                          Yield x s1' -> Yield x (Left s1')
    next (Right s2) = case next1 s2 of
                          Done        -> Done
                          Skip s2'    -> Skip    (Right s2')
                          Yield x s2' -> Yield x (Right s2')
{-# INLINE [0] append #-}

-- version that can share the second list arg, really very similar
-- to unstream, but conses onto a given list rather than []:
-- unstream s = append1 s []
--
append1 :: Stream a -> [a] -> [a]
append1 (Stream next s0) xs = loop_append1 s0
  where
    loop_append1 !s = case next s of
      Done       -> xs
      Skip    s' -> expose s'       loop_append1 s'
      Yield x s' -> expose s' $ x : loop_append1 s'
{-# INLINE [0] append1 #-}

snoc :: Stream a -> a -> Stream a
snoc (Stream next0 xs0) w = Stream next (Just xs0)
  where
    {-# INLINE next #-}
    next (Just xs) = case next0 xs of
      Done        -> Yield w Nothing
      Skip xs'    -> Skip    (Just xs')
      Yield x xs' -> Yield x (Just xs')
    next Nothing = Done
{-# INLINE [0] snoc #-}

cons :: a -> Stream a -> Stream a
cons w (Stream next0 s0) = Stream next (S2 :!: s0)
  where
    {-# INLINE next #-}
    next (S2 :!: s) = Yield w (S1 :!: s)
    next (S1 :!: s) = case next0 s of
        Done       -> Done
        Skip    s' -> Skip    (S1 :!: s')
        Yield x s' -> Yield x (S1 :!: s')
{-# INLINE [0] cons #-}

-- head
head :: Stream a -> a
head (Stream next s0) = loop_head s0
  where
    loop_head !s = case next s of
        Yield x _  -> x
        Skip    s' -> expose s' $ loop_head s'
        Done       -> errorEmptyStream "head"
{-# INLINE [0] head #-}

-- last
last :: Stream a -> a
last (Stream next s0) = loop0_last s0
  where
    loop0_last !s = case next s of
      Done       -> errorEmptyStream "last"
      Skip    s' -> expose s' $ loop0_last  s'
      Yield x s' -> expose s' $ loop_last x s'
    loop_last x !s = case next s of
      Done        -> x
      Skip     s' -> expose s' $ loop_last x  s'
      Yield x' s' -> expose s' $ loop_last x' s'
{-# INLINE [0] last #-}

-- tail
tail :: Stream a -> Stream a
tail (Stream next0 s0) = Stream next (S1 :!: s0)
  where
    {-# INLINE next #-}
    next (S1 :!: s) = case next0 s of
      Done       -> errorEmptyStream "tail"
      Skip    s' -> Skip (S1 :!: s')
      Yield _ s' -> Skip (S2 :!: s') -- drop the head
    next (S2 :!: s) = case next0 s of
      Done       -> Done
      Skip    s' -> Skip    (S2 :!: s')
      Yield x s' -> Yield x (S2 :!: s')
{-# INLINE [0] tail #-}

-- init
init :: Stream a -> Stream a
init (Stream next0 s0) = Stream next (Nothing :!: s0)
  where
    {-# INLINE next #-}
    next (Nothing :!: s) = case next0 s of
      Done       -> errorEmptyStream "init"
      Skip    s' -> Skip (Nothing    :!: s')
      Yield x s' -> Skip (Just (L x) :!: s')
    next (Just (L x) :!: s) = case next0 s of
      Done        -> Done
      Skip     s' -> Skip    (Just (L x)  :!: s')
      Yield x' s' -> Yield x (Just (L x') :!: s')
{-# INLINE [0] init #-}

-- null
null :: Stream a -> Bool
null (Stream next s0) = loop_null s0
  where
    loop_null !s = case next s of
      Done       -> True
      Yield _ _  -> False
      Skip    s' -> expose s' $ loop_null s'
{-# INLINE [0] null #-}

-- length
length :: Stream a -> Int
length (Stream next s0) = loop_length (0::Int) s0
  where
    loop_length !z !s = case next s of
      Done       -> z
      Skip    s' -> expose s' $ loop_length  z    s'
      Yield _ s' -> expose s' $ loop_length (z+1) s'
{-# INLINE [0] length #-}

{-
-- For lazy bytestrings
length64 :: Stream a -> Int64
length64 (Stream next s0) = loop (0::Int64) s0
  where
    loop z !s = case next s of
      Done       -> z
      Skip    s' -> loop  z    s'
      Yield _ s' -> loop (z+1) s'
{-# INLINE [0] length64 #-}
-}

-- ---------------------------------------------------------------------
-- Stream transformations

-- map
map :: (a -> b) -> Stream a -> Stream b
map f (Stream next0 s0) = Stream next s0
  where
    {-# INLINE next #-}
    next !s = case next0 s of
        Done       -> Done
        Skip    s' -> Skip        s'
        Yield x s' -> Yield (f x) s'
{-# INLINE [0] map #-}

--
-- a convenient rule for map
--
{-# RULES
    "STREAM map/map fusion" forall f g s.
        map f (map g s) = map (\x -> f (g x)) s
 #-}

--
-- relies strongly on SpecConstr
--

intersperse :: a -> Stream a -> Stream a
intersperse sep (Stream next0 s0) = Stream next (s0 :!: Nothing :!: S1)
  where
    {-# INLINE next #-}
    next (s :!: Nothing :!: S1) = case next0 s of
      Done       -> Done
      Skip    s' -> Skip (s' :!: Nothing    :!: S1)
      Yield x s' -> Skip (s' :!: Just (L x) :!: S1)

    next (s :!: Just (L x) :!: S1) = Yield x (s :!: Nothing :!: S2)

    next (s :!: Nothing :!: S2) = case next0 s of
      Done       -> Done
      Skip    s' -> Skip      (s' :!: Nothing    :!: S2)
      Yield x s' -> Yield sep (s' :!: Just (L x) :!: S1)

    -- next (_ :!: (Just (L _))) :!: S2 -- can't happen

{-
intersperse :: a -> Stream a -> [a]
intersperse sep (Stream next s0) = loop_intersperse_start s0
  where
    loop_intersperse_start !s = case next s of
      Done       -> []
      Skip    s' -> expose s' $     loop_intersperse_start s'
      Yield x s' -> expose s' $ x : loop_intersperse_go    s'

    loop_intersperse_go !s = case next s of
      Done       -> []
      Skip    s' -> expose s' $           loop_intersperse_go s'
      Yield x s' -> expose s' $ sep : x : loop_intersperse_go s'
-}

-- intercalate :: Stream a -> Stream (Stream a) -> Stream a
-- transpose :: Stream (Stream a) -> Stream (Stream a)

------------------------------------------------------------------------

-- * Reducing streams (folds)

foldl :: (b -> a -> b) -> b -> Stream a -> b
foldl f z0 (Stream next s0) = loop_foldl z0 s0
  where
    loop_foldl z !s = case next s of
      Done       -> z
      Skip    s' -> expose s' $ loop_foldl z s'
      Yield x s' -> expose s' $ loop_foldl (f z x) s'
{-# INLINE [0] foldl #-}

foldl' :: (b -> a -> b) -> b -> Stream a -> b
foldl' f z0 (Stream next s0) = loop_foldl' z0 s0
  where
    loop_foldl' !z !s = case next s of
      Done       -> z
      Skip    s' -> expose s' $ loop_foldl' z s'
      Yield x s' -> expose s' $ loop_foldl' (f z x) s'
{-# INLINE [0] foldl' #-}

foldl1 :: (a -> a -> a) -> Stream a -> a
foldl1 f (Stream next s0) = loop0_foldl1 s0
  where
    loop0_foldl1 !s = case next s of
      Skip    s' -> expose s' $ loop0_foldl1 s'
      Yield x s' -> expose s' $ loop_foldl1 x s'
      Done       -> errorEmptyStream "foldl1"

    loop_foldl1 z !s = expose s $ case next s of
      Done       -> z
      Skip    s' -> expose s' $ loop_foldl1 z s'
      Yield x s' -> expose s' $ loop_foldl1 (f z x) s'
{-# INLINE [0] foldl1 #-}

foldl1' :: (a -> a -> a) -> Stream a -> a
foldl1' f (Stream next s0) = loop0_foldl1' s0
  where
    loop0_foldl1' !s = case next s of
      Skip    s' -> expose s' $ loop0_foldl1' s'
      Yield x s' -> expose s' $ loop_foldl1' x s'
      Done       -> errorEmptyStream "foldl1"

    loop_foldl1' !z !s = case next s of
      Done       -> z
      Skip    s' -> expose s' $ loop_foldl1' z s'
      Yield x s' -> expose s' $ loop_foldl1' (f z x) s'
{-# INLINE [0] foldl1' #-}

foldr :: (a -> b -> b) -> b -> Stream a -> b
foldr f z (Stream next s0) = loop_foldr s0
  where
    loop_foldr !s = case next s of
      Done       -> z
      Skip    s' -> expose s' $ loop_foldr s'
      Yield x s' -> expose s' $ f x (loop_foldr s')
{-# INLINE [0] foldr #-}

foldr1 :: (a -> a -> a) -> Stream a -> a
foldr1 f (Stream next s0) = loop0_foldr1 s0
  where
    loop0_foldr1 !s = case next s of
      Done       -> errorEmptyStream "foldr1"
      Skip    s' -> expose s' $ loop0_foldr1  s'
      Yield x s' -> expose s' $ loop_foldr1 x s'

    loop_foldr1 x !s = case next s of
      Done        -> x
      Skip     s' -> expose s' $ loop_foldr1 x s'
      Yield x' s' -> expose s' $ f x (loop_foldr1 x' s')
{-# INLINE [0] foldr1 #-}

------------------------------------------------------------------------
-- ** Special folds

-- concat
--

concat :: Stream [a] -> [a]
concat (Stream next s0) = loop_concat_to s0
  where
    loop_concat_go []     !s = expose s $     loop_concat_to    s
    loop_concat_go (x:xs) !s = expose s $ x : loop_concat_go xs s

    loop_concat_to !s = case next s of
      Done        -> []
      Skip     s' -> expose s' $ loop_concat_to    s'
      Yield xs s' -> expose s' $ loop_concat_go xs s'
{-# INLINE [0] concat #-}

{-
concat :: Stream [a] -> Stream a
concat (Stream next0 s0) = Stream next (Nothing :!: s0)
  where
    {-# INLINE next #-}
    next (Just (L [])     :!: s) = expose s $ Skip    (Nothing     :!: s)
    next (Just (L (x:xs)) :!: s) = expose s $ Yield x (Just (L xs) :!: s)

    next (Nothing :!: s) = case next0 s of
      Done        -> Done
      Skip     s' -> expose s' $ Skip (Nothing     :!: s')
      Yield xs s' -> expose s' $ Skip (Just (L xs) :!: s')
-}

{-
concatMap :: (a -> [b]) -> Stream a -> [b]
concatMap f (Stream next s0) = loop_concatMap_to s0
  where
    loop_concatMap_go []     !s = expose s $     loop_concatMap_to    s
    loop_concatMap_go (b:bs) !s = expose s $ b : loop_concatMap_go bs s

    loop_concatMap_to !s = case next s of
      Done       -> []
      Skip    s' -> expose s' $ loop_concatMap_to       s'
      Yield a s' -> expose s' $ loop_concatMap_go (f a) s'
{-# INLINE [0] concatMap #-}
-}

{-
concatMap :: (a -> [b]) -> Stream a -> Stream b
concatMap f (Stream next0 s0) = Stream next (Nothing :!: s0)
  where
    {-# INLINE next #-}
    next (Just (L [])     :!: s) = expose s $ Skip    (Nothing     :!: s)
    next (Just (L (b:bs)) :!: s) = expose s $ Yield b (Just (L bs) :!: s)

    next (Nothing :!: s) = case next0 s of
      Done       -> Done
      Skip    s' -> expose s' $ Skip (Nothing        :!: s')
      Yield a s' -> expose s' $ Skip (Just (L (f a)) :!: s')

-}
{-
Here's an approach to fusing concatMap fully:
we try and match the Stream inside in the argument to concatMap and pass that
directly to a concatMap' variant. The point here is that the step function does
not depend on 'x', something which the rule below does not enforce :-)
-}

{- RULES
"dodgy concatMap rule"   forall step f.
  concatMap (\x -> unstream (Stream step (f x))) = \y -> unstream (concatMap' step f y)
  -}

{-
concatMap' :: Unlifted s => (s -> Step b s) -> (a -> s) -> Stream a -> Stream b
concatMap' nextb f (Stream nexta sa0) = Stream next (sa0 :!: Nothing)
  where
    {-# INLINE next #-}
    next (sa :!: Just sb) = case nextb sb of
      Done        -> Skip    (sa :!: Nothing)
      Skip    sb' -> Skip    (sa :!: Just sb')
      Yield b sb' -> Yield b (sa :!: Just sb')

    next (sa :!: Nothing) = case nexta sa of
      Done        -> Done
      Skip    sa' -> Skip (sa' :!: Nothing)
      Yield a sa' -> Skip (sa' :!: Just (f a))
-}

{-
-- note the nested stream is a little hard to construct in a fusible
-- manner
-- 
concat :: Stream (Stream a) -> Stream a
concat (Stream next0 s0) = Stream next (Right s0)
  where
    {-# INLINE next #-}
    next (Left (Stream f t :!: s)) = case f t of
      Done       -> Skip    (Right s)
      Skip    t' -> Skip    (Left (Stream f t' :!: s))
      Yield x t' -> Yield x (Left (Stream f t' :!: s))
    next (Right s) = case next0 s of
      Done       -> Done
      Skip    s' -> Skip (Right s')
      Yield x s' -> Skip (Left (x :!: s'))
{-# INLINE [0] concat #-}
-}

concatMap :: (a -> Stream b) -> Stream a -> Stream b
concatMap f (Stream next0 s0) = Stream next (s0 :!: Nothing)
  where
    {-# INLINE next #-}
    next (s :!: Nothing) = case next0 s of
      Done       -> Done
      Skip    s' -> Skip (s' :!: Nothing)
      Yield x s' -> Skip (s' :!: Just (f x))

    next (s :!: Just (Stream g t)) = case g t of
      Done       -> Skip    (s :!: Nothing)
      Skip    t' -> Skip    (s :!: Just (Stream g t'))
      Yield x t' -> Yield x (s :!: Just (Stream g t'))

{-# INLINE [0] concatMap #-}

and :: Stream Bool -> Bool
and = foldr (&&) True
{-# INLINE and #-}

or :: Stream Bool -> Bool
or = foldr (||) False
{-# INLINE or #-}

any :: (a -> Bool) -> Stream a -> Bool
any p (Stream next s0) = loop_any s0
  where
    loop_any !s = case next s of
      Done                   -> False
      Skip    s'             -> expose s' $ loop_any s'
      Yield x s' | p x       -> True
                 | otherwise -> expose s' $ loop_any s'
{-# INLINE [0] any #-}

all :: (a -> Bool) -> Stream a -> Bool
all p (Stream next s0) = loop_all s0
  where
    loop_all !s = case next s of
      Done                   -> True
      Skip    s'             -> expose s' $ loop_all s'
      Yield x s' | p x       -> expose s' $ loop_all s'
                 | otherwise -> False
{-# INLINE [0] all #-}

sum :: Num a => Stream a -> a
sum (Stream next s0) = loop_sum 0 s0
  where
    loop_sum !a !s = case next s of   -- note: strict in the accumulator!
      Done       -> a
      Skip    s' -> expose s' $ loop_sum a s'
      Yield x s' -> expose s' $ loop_sum (a + x) s'
{-# INLINE [0] sum #-}

product :: Num a => Stream a -> a
product (Stream next s0) = loop_product 1 s0   -- note: strict in the accumulator!
  where
    loop_product !a !s = case next s of
      Done       -> a
      Skip    s' -> expose s' $ loop_product a s'
      Yield x s' -> expose s' $ loop_product (a * x) s'
{-# INLINE [0] product #-}

maximum :: Ord a => Stream a -> a
maximum (Stream next s0) = loop0_maximum s0
  where
    loop0_maximum !s = case next s of
      Done       -> errorEmptyStream "maximum"
      Skip    s' -> expose s' $ loop0_maximum s'
      Yield x s' -> expose s' $ loop_maximum x s'
    loop_maximum z !s = case next s of               -- note, lazy in the accumulator
      Done       -> z
      Skip    s' -> expose s' $ loop_maximum z s'
      Yield x s' -> expose s' $ loop_maximum (max z x) s'
{-# INLINE [0] maximum #-}

{-# RULES
  "maximumInt"     maximum = (strictMaximum :: Stream Int  -> Int);
  "maximumChar"    maximum = (strictMaximum :: Stream Char -> Char)
  #-}

strictMaximum :: Ord a => Stream a -> a
strictMaximum (Stream next s0) = loop0_strictMaximum s0
  where
    loop0_strictMaximum !s = case next s of
      Done       -> errorEmptyStream "maximum"
      Skip    s' -> expose s' $ loop0_strictMaximum s'
      Yield x s' -> expose s' $ loop_strictMaximum x s'
    loop_strictMaximum !z !s = case next s of
      Done       -> z
      Skip    s' -> expose s' $ loop_strictMaximum z s'
      Yield x s' -> expose s' $ loop_strictMaximum (max z x) s'
{-# INLINE [0] strictMaximum #-}

minimum :: Ord a => Stream a -> a
minimum (Stream next s0) = loop0_minimum s0
  where
    loop0_minimum !s = case next s of
      Done       -> errorEmptyStream "minimum"
      Skip    s' -> expose s' $ loop0_minimum s'
      Yield x s' -> expose s' $ loop_minimum x s'
    loop_minimum z !s = case next s of
      Done       -> z
      Skip    s' -> expose s' $ loop_minimum z s'
      Yield x s' -> expose s' $ loop_minimum (min z x) s'
{-# INLINE [0] minimum #-}

{-# RULES
  "minimumInt"     minimum = (strictMinimum :: Stream Int  -> Int);
  "minimumChar"    minimum = (strictMinimum :: Stream Char -> Char)
  #-}

strictMinimum :: Ord a => Stream a -> a
strictMinimum (Stream next s0) = loop0_strictMinimum s0
  where
    loop0_strictMinimum !s = case next s of
      Done       -> errorEmptyStream "minimum"
      Skip    s' -> expose s' $ loop0_strictMinimum s'
      Yield x s' -> expose s' $ loop_strictMinimum x s'
    loop_strictMinimum !z !s = case next s of
      Done       -> z
      Skip    s' -> expose s' $ loop_strictMinimum z s'
      Yield x s' -> expose s' $ loop_strictMinimum (min z x) s'
{-# INLINE [0] strictMinimum #-}

------------------------------------------------------------------------
-- * Building lists
-- ** Scans

--
-- FIXME: not a proper scanl. expects a list one longer than the input list,
-- in order to get the z0th element
--
scanl :: (b -> a -> b) -> b -> Stream a -> Stream b
scanl f z0 (Stream next0 s0) = Stream next (L z0 :!: s0)
  where
    {-# INLINE next #-}
    next (L z :!: s) = case next0 s of
        Done        -> Done
        Skip    s'  -> Skip    (L z       :!: s')
        Yield x s'  -> Yield z (L (f z x) :!: s')
{-# INLINE [0] scanl #-}

scanl1 :: (a -> a -> a) -> Stream a -> Stream a
scanl1 f (Stream next0 s0) = Stream next (Nothing :!: s0)
  where
    {-# INLINE next #-}
    next (Nothing :!: s)      = case next0 s of
            Done       -> Done
            Skip s'    -> Skip (Nothing    :!: s')
            Yield x s' -> Skip (Just (L x) :!: s')
    next (Just (L z) :!: s)   = case next0 s of
        Done       -> Done
        Skip    s' -> Skip    (Just (L z)       :!: s')
        Yield x s' -> Yield z (Just (L (f z x)) :!: s')
{-# INLINE [0] scanl1 #-}

--
-- hmm. hard.
--

{-
scanr :: (b -> a -> b) -> b -> Stream a -> Stream b
scanr f z0 (Stream next s0) = Stream next' (Just s0)
  where
    next' (Just s) = case next s of
        Done        -> Yield z0 (Nothing, s)
        Skip s'     -> Skip (Just s')
        Yield x s'  -> -- hmm.

    next' Nothing = Done
{-# INLINE [0] scanl #-}
-}


{-
scanr :: (a -> b -> b) -> b -> Stream a -> Stream b
scanr f z0 (Stream next s0) = Stream next' (z0, s0)     -- should be using strict pairs??
  where
    next' (z, s) = case next s of
        Done        -> Done
        Skip s'     -> Skip (z, s')
        Yield x s'  -> Yield z (f x z, s') -- flip f
{-# INLINE [0] scanr #-}
-}

{-
scanl1 :: (a -> a -> a) -> Stream a -> Stream a
scanr1 :: (a -> a -> a) -> Stream a -> Stream a
-}

------------------------------------------------------------------------
-- ** Accumulating maps

{-
--
-- not right:
--
mapAccumL :: (acc -> x -> (acc, y)) -> acc -> Stream x -> (acc, Stream y)
mapAccumL f acc (Stream step s) = Stream step' (s, acc)
  where
    step' (s, acc) = case step s of
          Done       -> Done
          Skip s'    -> Skip (s', acc)
          Yield x s' -> let (acc', y) = f acc x in Yield y (s', acc')
{-# INLINE [0] mapAccumL #-}
-}

{-
mapAccumR :: (acc -> x -> (acc, y)) -> acc -> Stream x -> (acc, Stream y)
-}

------------------------------------------------------------------------
-- ** Infinite streams

iterate :: (a -> a) -> a -> Stream a
iterate f x0 = Stream next (L x0)
  where
    {-# INLINE next #-}
    next (L x) = Yield x (L (f x))
{-# INLINE [0] iterate #-}

repeat :: a -> Stream a
repeat x = Stream next None
  where
    {-# INLINE next #-}
    next _ = Yield x None
{-# INLINE [0] repeat #-}

{-# RULES
  "map/repeat" forall f x. map f (repeat x) = repeat (f x)
  #-}

replicate :: Int -> a -> Stream a
replicate n x = Stream next (L n)
  where
    {-# INLINE next #-}
    next (L !i) | i <= 0    = Done
                | otherwise = Yield x (L (i-1))
{-# INLINE [0] replicate #-}

{-# RULES
  "map/replicate" forall f n x. map f (replicate n x) = replicate n (f x)
  #-}

--"reverse/replicate" forall n x. reverse (replicate n x) = replicate n x

cycle :: Stream a -> Stream a
cycle (Stream next0 s0) = Stream next (s0 :!: S1)
  where
    {-# INLINE next #-}
    next (s :!: S1) = case next0 s of
      Done       -> errorEmptyStream "cycle"
      Skip    s' -> Skip    (s' :!: S1)
      Yield x s' -> Yield x (s' :!: S2)
    next (s :!: S2) = case next0 s of
      Done       -> Skip    (s0 :!: S2)
      Skip    s' -> Skip    (s' :!: S2)
      Yield x s' -> Yield x (s' :!: S2)
{-# INLINE [0] cycle #-}

------------------------------------------------------------------------
-- ** Unfolding

unfoldr :: (b -> Data.Maybe.Maybe (a, b)) -> b -> Stream a
unfoldr f s0 = Stream next (L s0)
  where
    {-# INLINE next #-}
    next (L s) = case f s of
      Data.Maybe.Nothing      -> Done
      Data.Maybe.Just (w, s') -> Yield w (L s')
{-# INLINE [0] unfoldr #-}

------------------------------------------------------------------------
-- * Substreams
-- ** Extracting substreams

take :: Int -> Stream a -> Stream a
take n0 (Stream next0 s0) = Stream next (L n0 :!: s0)
  where
    {-# INLINE next #-}
    next (L !n :!: s)
      | n <= 0    = Done
      | otherwise = case next0 s of
            Done       -> Done
            Skip    s' -> Skip    (L  n    :!: s')
            Yield x s' -> Yield x (L (n-1) :!: s')
{-# INLINE [0] take #-}

drop :: Int -> Stream a -> Stream a
drop n0 (Stream next0 s0) = Stream next (Just (L (max 0 n0)) :!: s0)
  where
    {-# INLINE next #-}
    next (Just (L !n) :!: s)
      | n == 0    = Skip (Nothing :!: s)
      | otherwise = case next0 s of
          Done       -> Done
          Skip    s' -> Skip (Just (L  n)    :!: s')
          Yield _ s' -> Skip (Just (L (n-1)) :!: s')
    next (Nothing :!: s) = case next0 s of
      Done       -> Done
      Skip    s' -> Skip    (Nothing :!: s')
      Yield x s' -> Yield x (Nothing :!: s')
{-# INLINE [0] drop #-}

--TODO: could perhaps use 0 instead of Nothing, so long as
--      spec constr works with that

splitAt :: Int -> Stream a -> ([a], [a])
splitAt n0 (Stream next s0)
  --TODO: we should not need this special case, (n < 0) should be as
  --      cheap as pattern matching n against 0
  | n0 < 0    = ([], expose s0 $ unstream (Stream next s0))
  | otherwise = loop_splitAt n0 s0
  where
    loop_splitAt  0 !s = ([], expose s $ unstream (Stream next s))
    loop_splitAt !n !s = case next s of
      Done            -> ([], [])
      Skip    s'      -> expose s $ loop_splitAt n s'
      Yield x s'      -> (x:xs', xs'')
        where
          (xs', xs'') = expose s $ loop_splitAt (n-1) s'
{-# INLINE [0] splitAt #-}

takeWhile :: (a -> Bool) -> Stream a -> Stream a
takeWhile p (Stream next0 s0) = Stream next s0
  where
    {-# INLINE next #-}
    next !s = case next0 s of
      Done                   -> Done
      Skip    s'             -> Skip s'
      Yield x s' | p x       -> Yield x s'
                 | otherwise -> Done
{-# INLINE [0] takeWhile #-}

dropWhile :: (a -> Bool) -> Stream a -> Stream a
dropWhile p (Stream next0 s0) = Stream next (S1 :!: s0)
  where
    {-# INLINE next #-}
    next (S1 :!: s)  = case next0 s of
      Done                   -> Done
      Skip    s'             -> Skip    (S1 :!: s')
      Yield x s' | p x       -> Skip    (S1 :!: s')
                 | otherwise -> Yield x (S2 :!: s')
    next (S2 :!: s) = case next0 s of
      Done       -> Done
      Skip    s' -> Skip    (S2 :!: s')
      Yield x s' -> Yield x (S2 :!: s')
{-# INLINE [0] dropWhile #-}

{-
span :: (a -> Bool) -> Stream a -> (Stream a, Stream a)
break :: (a -> Bool) -> Stream a -> (Stream a, Stream a)
group :: Eq a => Stream a -> Stream (Stream a)
inits :: Stream a -> Stream (Stream a)
tails :: Stream a -> Stream (Stream a)
-}

------------------------------------------------------------------------
-- * Predicates

isPrefixOf :: Eq a => Stream a -> Stream a -> Bool
isPrefixOf (Stream stepa sa0) (Stream stepb sb0) = loop_isPrefixOf sa0 sb0 Nothing
  where
    loop_isPrefixOf !sa !sb Nothing = case stepa sa of
        Done        -> True
        Skip    sa' -> expose sa' $ loop_isPrefixOf sa' sb Nothing
        Yield x sa' -> expose sa' $ loop_isPrefixOf sa' sb (Just (L x))

    loop_isPrefixOf !sa !sb (Just (L x)) = case stepb sb of
        Done                    -> False
        Skip    sb'             -> expose sb' $ loop_isPrefixOf sa sb' (Just (L x))
        Yield y sb' | x == y    -> expose sb' $ loop_isPrefixOf sa sb' Nothing
                    | otherwise -> False
{-# INLINE [0] isPrefixOf #-}


{-
isSuffixOf :: Eq a => Stream a -> Stream a -> Bool
isInfixOf :: Eq a => Stream a -> Stream a -> Bool
-}

------------------------------------------------------------------------
-- * Searching streams
-- ** Searching by equality

elem :: Eq a => a -> Stream a -> Bool
elem x (Stream next s0) = loop_elem s0
  where
    loop_elem !s = case next s of
      Done          -> False
      Skip    s'    -> expose s' $            loop_elem s'
      Yield y s'
        | x == y    -> True
        | otherwise -> expose s' $ loop_elem s'
{-# INLINE [0] elem #-}

{-
--
-- No need to provide notElem, as not . elem is just as fusible.
-- You can only fuse on the rhs of elem anyway.
--

notElem :: Eq a => a -> Stream a -> Bool
notElem x (Stream next s0) = loop s0
  where
    loop !s = case next s of
      Done                   -> True
      Skip    s'             -> loop s'
      Yield y s' | x == y    -> False
                 | otherwise -> loop s'
{-# INLINE [0] notElem #-}
-}

lookup :: Eq a => a -> Stream (a, b) -> Data.Maybe.Maybe b
lookup key (Stream next s0) = loop_lookup s0
  where
    loop_lookup !s = case next s of
      Done                        -> Data.Maybe.Nothing
      Skip         s'             -> expose s' $ loop_lookup s'
      Yield (x, y) s' | key == x  -> Data.Maybe.Just y
                      | otherwise -> expose s' $ loop_lookup s'
{-# INLINE [0] lookup #-}

------------------------------------------------------------------------
-- ** Searching with a predicate

find :: (a -> Bool) -> Stream a -> Data.Maybe.Maybe a
find p (Stream next s0) = loop_find s0
  where
    loop_find !s = case next s of
      Done                   -> Data.Maybe.Nothing
      Skip    s'             -> expose s' $ loop_find s'
      Yield x s' | p x       -> Data.Maybe.Just x
                 | otherwise -> expose s' $ loop_find s'
{-# INLINE [0] find #-}

filter :: (a -> Bool) -> Stream a -> Stream a
filter p (Stream next0 s0) = Stream next s0
  where
    {-# INLINE next #-}
    next !s = case next0 s of
      Done                   -> Done
      Skip    s'             -> Skip    s'
      Yield x s' | p x       -> Yield x s'
                 | otherwise -> Skip    s'
{-# INLINE [0] filter #-}

{-# RULES
    "Stream filter/filter fusion" forall p q s.
        filter p (filter q s) = filter (\x -> q x && p x) s
 #-}

--partition :: (a -> Bool) -> Stream a -> (Stream a, Stream a)

------------------------------------------------------------------------
-- * Indexing streams

index :: Stream a -> Int -> a
index (Stream next s0) n0
  | n0 < 0    = error "Stream.(!!): negative index"
  | otherwise = loop_index n0 s0
  where
    loop_index !n !s = case next s of
      Done                   -> error "Stream.(!!): index too large"
      Skip    s'             -> expose s' $ loop_index  n    s'
      Yield x s' | n == 0    -> x
                 | otherwise -> expose s' $ loop_index (n-1) s'
{-# INLINE [0] index #-}

findIndex :: (a -> Bool) -> Stream a -> Data.Maybe.Maybe Int
findIndex p (Stream next s0) = loop_findIndex 0 s0
  where
    loop_findIndex !i !s = case next s of
      Done                   -> Data.Maybe.Nothing
      Skip    s'             -> expose s' $ loop_findIndex i     s' -- hmm. not caught by QC
      Yield x s' | p x       -> Data.Maybe.Just i
                 | otherwise -> expose s' $ loop_findIndex (i+1) s'
{-# INLINE [0] findIndex #-}

elemIndex :: Eq a => a -> Stream a -> Data.Maybe.Maybe Int
elemIndex a (Stream next s0) = loop_elemIndex 0 s0
  where
    loop_elemIndex !i !s = case next s of
      Done                   -> Data.Maybe.Nothing
      Skip    s'             -> expose s' $ loop_elemIndex i     s'
      Yield x s' | a == x    -> Data.Maybe.Just i
                 | otherwise -> expose s' $ loop_elemIndex (i+1) s'
{-# INLINE [0] elemIndex #-}

elemIndices :: Eq a => a -> Stream a -> Stream Int
elemIndices a (Stream next0 s0) = Stream next (S 0 :!: s0)
  where
    {-# INLINE next #-}
    next (S n :!: s) = case next0 s of
        Done                   -> Done
        Skip    s'             -> Skip    (S n     :!: s')
        Yield x s' | x == a    -> Yield n (S (n+1) :!: s')
                   | otherwise -> Skip    (S (n+1) :!: s')
{-# INLINE [0] elemIndices #-}

findIndices :: (a -> Bool) -> Stream a -> Stream Int
findIndices p (Stream next0 s0) = Stream next (S 0 :!: s0)
  where
    {-# INLINE next #-}
    next (S n :!: s) = case next0 s of
        Done                   -> Done
        Skip    s'             -> Skip    (S n     :!: s')
        Yield x s' | p x       -> Yield n (S (n+1) :!: s')
                   | otherwise -> Skip    (S (n+1) :!: s')
{-# INLINE [0] findIndices #-}

------------------------------------------------------------------------
-- * Zipping and unzipping streams

zip :: Stream a -> Stream b -> Stream (a, b)
zip = zipWith (,)
{-# INLINE zip #-}

zip3 :: Stream a -> Stream b -> Stream c -> Stream (a, b, c)
zip3 = zipWith3 (,,)
{-# INLINE zip3 #-}

zip4 :: Stream a -> Stream b -> Stream c -> Stream d -> Stream (a, b, c, d)
zip4 = zipWith4 (,,,)
{-# INLINE zip4 #-}

{-
zip5 :: Stream a -> Stream b -> Stream c -> Stream d -> Stream e -> [(a, b, c, d, e)]
zip6 :: Stream a -> Stream b -> Stream c -> Stream d -> Stream e -> Stream f -> [(a, b, c, d, e, f)]
zip7 :: Stream a -> Stream b -> Stream c -> Stream d -> Stream e -> Stream f -> Stream g -> [(a, b, c, d, e, f, g)]
-}

zipWith :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWith f (Stream next0 sa0) (Stream next1 sb0) = Stream next (sa0 :!: sb0 :!: Nothing)
  where
    {-# INLINE next #-}
    next (sa :!: sb :!: Nothing)     = case next0 sa of
        Done        -> Done
        Skip    sa' -> Skip (sa' :!: sb :!: Nothing)
        Yield a sa' -> Skip (sa' :!: sb :!: Just (L a))

    next (sa' :!: sb :!: Just (L a)) = case next1 sb of
        Done        -> Done
        Skip    sb' -> Skip          (sa' :!: sb' :!: Just (L a))
        Yield b sb' -> Yield (f a b) (sa' :!: sb' :!: Nothing)
{-# INLINE [0] zipWith #-}

zipWith3 :: (a -> b -> c -> d) -> Stream a -> Stream b -> Stream c -> Stream d
zipWith3 f (Stream nexta sa0)
           (Stream nextb sb0)
           (Stream nextc sc0) = Stream next (sa0 :!: sb0 :!: sc0 :!: Nothing)
  where
    {-# INLINE next #-}
    next (sa :!:  sb :!:  sc :!: Nothing) = case nexta sa of
      Done        -> Done
      Skip    sa' -> Skip (sa' :!: sb :!: sc :!: Nothing)
      Yield a sa' -> Skip (sa' :!: sb :!: sc :!: Just (L a :!: Nothing))

    next (sa' :!: sb :!:  sc :!: Just (L a :!: Nothing)) = case nextb sb of
      Done        -> Done
      Skip    sb' -> Skip          (sa' :!: sb' :!: sc :!: Just (L a :!: Nothing))
      Yield b sb' -> Skip          (sa' :!: sb' :!: sc :!: Just (L a :!: Just (L b)))

    next (sa' :!: sb' :!: sc :!: Just (L a :!: Just (L b))) = case nextc sc of
      Done        -> Done
      Skip    sc' -> Skip            (sa' :!: sb' :!: sc' :!: Just (L a :!: Just (L b)))
      Yield c sc' -> Yield (f a b c) (sa' :!: sb' :!: sc' :!: Nothing)
{-# INLINE [0] zipWith3 #-}

zipWith4 :: (a -> b -> c -> d -> e) -> Stream a -> Stream b -> Stream c -> Stream d -> Stream e
zipWith4 f (Stream nexta sa0)
           (Stream nextb sb0)
           (Stream nextc sc0)
           (Stream nextd sd0) = Stream next (sa0 :!: sb0 :!: sc0 :!: sd0 :!: Nothing)
  where
    {-# INLINE next #-}
    next (sa :!:  sb :!:  sc :!: sd :!: Nothing) =
        case nexta sa of
            Done        -> Done
            Skip    sa' -> Skip (sa' :!: sb :!: sc :!: sd :!: Nothing)
            Yield a sa' -> Skip (sa' :!: sb :!: sc :!: sd :!: Just (L a :!: Nothing))

    next (sa' :!: sb :!:  sc :!: sd :!: Just (L a :!: Nothing)) =
        case nextb sb of
            Done        -> Done
            Skip    sb' -> Skip (sa' :!: sb' :!: sc :!: sd :!: Just (L a :!: Nothing))
            Yield b sb' -> Skip (sa' :!: sb' :!: sc :!: sd :!: Just (L a :!: Just (L b :!: Nothing)))

    next (sa' :!: sb' :!: sc :!: sd :!: Just (L a :!: (Just (L b :!: Nothing)))) =
        case nextc sc of
            Done        -> Done
            Skip    sc' -> Skip (sa' :!: sb' :!: sc' :!: sd :!: Just (L a :!: (Just (L b :!: Nothing))))
            Yield c sc' -> Skip (sa' :!: sb' :!: sc' :!: sd :!: Just (L a :!: (Just (L b :!: Just (L c)))))

    next (sa' :!: sb' :!: sc' :!: sd :!: Just (L a :!: (Just (L b :!: Just (L c))))) =
        case nextd sd of
            Done        -> Done
            Skip    sd' -> Skip              (sa' :!: sb' :!: sc' :!: sd' :!: Just (L a :!: (Just (L b :!: Just (L c)))))
            Yield d sd' -> Yield (f a b c d) (sa' :!: sb' :!: sc' :!: sd' :!: Nothing)
{-# INLINE [0] zipWith4 #-}

unzip :: Stream (a, b) -> ([a], [b])
unzip = foldr (\(a,b) ~(as, bs) -> (a:as, b:bs)) ([], [])
{-# INLINE unzip #-}

------------------------------------------------------------------------
-- * Special streams
-- ** Functions on strings

{-
--
-- As a concatMap (snoc '\n')
--
unlines :: Stream (Stream Char) -> Stream Char
unlines (Stream next s0) = Stream next' (Right s0)
  where
    next' (Left (Stream g t, s)) = case g t of
      Done       -> Skip    (Right s)
      Skip    t' -> Skip    (Left (Stream g t', s))
      Yield x t' -> Yield x (Left (Stream g t', s))

    next' (Right s) = case next s of
      Done       -> Done
      Skip    s' -> Skip (Right s')
      Yield x s' -> Skip (Left ((snoc x '\n'), s'))
{-# INLINE [0] unlines #-}
-}

{-
--
-- As a concat . intersperse
--
unlines (Stream next s0) = Stream next' (Right s0)
  where
    -- go
    next' (Left (Stream f t, s)) = case f t of
      Done       -> Yield '\n' (Right s)
      Skip    t' -> Skip    (Left (Stream f t', s))
      Yield x t' -> Yield x (Left (Stream f t', s))

    -- to
    next' (Right s) = case next s of
      Done       -> Done
      Skip    s' -> Skip (Right s')
      Yield x s' -> Skip (Left (x, s'))
-}

{-
lines :: Stream Char -> Stream [Char]
lines (Stream next0 s0) = Stream next (Nothing :!: s0)
  where
    {-# INLINE next #-}
    next (Nothing  :!: s)     = case next0 s of
        Done           -> Done
        Skip s'        -> Skip (Nothing     :!: s')
        Yield _ _      -> Skip (Just (S []) :!: s) -- !

    next (Just (S acc) :!: s) = case next0 s of
        Done          -> Yield (reverse acc) (Nothing :!: s) -- !
        Skip s'       -> Skip (Just (S acc) :!: s')
        Yield '\n' s' -> Yield (reverse acc) (Nothing :!: s') -- reuse first state
        Yield x    s' -> Skip (Just (S (x:acc)) :!: s')

    {-# INLINE reverse #-}
    reverse :: [Char] -> [Char]
    reverse l = rev l []
      where
        rev []     a = a
        rev (x:xs) a = rev xs (x:a)
-}
{-
lines :: Stream Char -> Stream (Stream Char)
lines (Stream next s0 len) = Stream next' s0 len
  where
    next' s = case next s of
        Done    -> Done
        Skip s' -> Skip s'
-}


{-
lines' [] = []
lines' s  = let (l, s') = break (== '\n') s
            in l : case s' of
                     []      -> []
                     (_:s'') -> lines' s''
-}

{-
words :: String -> [String]
unlines :: [String] -> String
unwords :: [String] -> String
-}

------------------------------------------------------------------------
-- ** \"Set\" operations

{-
nub :: Eq a => Stream a -> Stream a
delete :: Eq a => a -> Stream a -> Stream a
difference :: Eq a => Stream a -> Stream a -> Stream a
union :: Eq a => Stream a -> Stream a -> Stream a
intersect :: Eq a => Stream a -> Stream a -> Stream a
-}

-- ** Ordered streams 

{-
sort :: Ord a => Stream a -> Stream a
insert :: Ord a => a -> Stream a -> Stream a
-}

------------------------------------------------------------------------
-- * Generalized functions
-- ** The \"By\" operations
-- *** User-supplied equality (replacing an Eq context)

{-
nubBy :: (a -> a -> Bool) -> Stream a -> Stream a
deleteBy :: (a -> a -> Bool) -> a -> Stream a -> Stream a
deleteFirstsBy :: (a -> a -> Bool) -> Stream a -> Stream a -> Stream a
unionBy :: (a -> a -> Bool) -> Stream a -> Stream a -> Stream a
intersectBy :: (a -> a -> Bool) -> Stream a -> Stream a -> Stream a
groupBy :: (a -> a -> Bool) -> Stream a -> Stream (Stream a)
-}

------------------------------------------------------------------------
-- *** User-supplied comparison (replacing an Ord context)

{-
sortBy :: (a -> a -> Ordering) -> Stream a -> Stream a
-}

insertBy :: (a -> a -> Ordering) -> a -> Stream a -> Stream a
insertBy cmp x (Stream next0 s0) = Stream next (S2 :!: s0)
  where
    {-# INLINE next #-}
    -- find the insertion point
    next (S2 :!: s) = case next0 s of
        Done        -> Yield x (S1 :!: s) -- a snoc
        Skip    s'  -> Skip    (S2 :!: s')
        Yield y s' | GT == cmp x y -> Yield y (S2 :!: s')
                   | otherwise     -> Yield x (S1 :!: s)  -- insert

    -- we've inserted, now just yield the rest of the stream
    next (S1 :!: s) = case next0 s of
        Done       -> Done
        Skip    s' -> Skip    (S1 :!: s')
        Yield y s' -> Yield y (S1 :!: s')
{-# INLINE [0] insertBy #-}

maximumBy :: (a -> a -> Ordering) -> Stream a -> a
maximumBy cmp (Stream next s0) = loop0_maximumBy s0
  where
    loop0_maximumBy !s = case next s of
      Skip    s' -> expose s' $ loop0_maximumBy s'
      Yield x s' -> expose s' $ loop_maximumBy x s'
      Done       -> errorEmptyStream "maximumBy"

    loop_maximumBy z !s = case next s of
      Done       -> z
      Skip    s' -> expose s' $ loop_maximumBy z s'
      Yield x s' -> expose s' $ loop_maximumBy (max' z x) s'

    max' x y = case cmp x y of
                    GT -> x
                    _  -> y
{-# INLINE [0] maximumBy #-}

minimumBy :: (a -> a -> Ordering) -> Stream a -> a
minimumBy cmp (Stream next s0) = loop0_minimumBy s0
  where
    loop0_minimumBy !s = case next s of
      Skip    s' -> expose s' $ loop0_minimumBy s'
      Yield x s' -> expose s' $ loop_minimumBy x s'
      Done       -> errorEmptyStream "minimum"

    loop_minimumBy z !s = case next s of
      Done       -> z
      Skip    s' -> expose s' $ loop_minimumBy z s'
      Yield x s' -> expose s' $ loop_minimumBy (min' z x) s'

    min' x y = case cmp x y of
                    GT -> y
                    _  -> x
{-# INLINE [0] minimumBy #-}

------------------------------------------------------------------------
-- * The \"generic\" operations

-- length
genericLength :: Num i => Stream b -> i
genericLength (Stream next s0) = loop_genericLength s0
  where
    loop_genericLength !s = case next s of
      Done       -> 0
      Skip    s' -> expose s' $     loop_genericLength s'
      Yield _ s' -> expose s' $ 1 + loop_genericLength s'
{-# INLINE [0] genericLength #-}

--TODO: specialised generic Length for strict/atomic and associative Num
-- instances like Int and Integer

genericTake :: Integral i => i -> Stream a -> Stream a
genericTake n0 (Stream next0 s0) = Stream next (L n0 :!: s0)
  where
    {-# INLINE next #-}
    next (L 0 :!: _)  = Done
    next (L n :!: s)  = case next0 s of
        Done          -> Done
        Skip    s'    -> Skip    (L  n    :!: s')
        Yield x s'
          | n > 0     -> Yield x (L (n-1) :!: s')
          | otherwise -> error "List.genericTake: negative argument"
{-# INLINE [0] genericTake #-}

-- genericTake is defined so bizzarely!

genericDrop :: Integral i => i -> Stream a -> Stream a
genericDrop n0 (Stream next0 s0) = Stream next (Just (L n0) :!: s0)
  where
    {-# INLINE next #-}
    next (Just (L 0) :!: s) = Skip (Nothing :!: s)
    next (Just (L n) :!: s) = case next0 s of
      Done                    -> Done
      Skip    s'              -> Skip (Just (L  n)    :!: s')
      Yield _ s' | n > 0      -> Skip (Just (L (n-1)) :!: s')
                 | otherwise  -> error "List.genericDrop: negative argument"
    next (Nothing :!: s) = case next0 s of
      Done       -> Done
      Skip    s' -> Skip    (Nothing :!: s')
      Yield x s' -> Yield x (Nothing :!: s')
{-# INLINE [0] genericDrop #-}

genericIndex :: Integral a => Stream b -> a -> b
genericIndex (Stream next s0) i0 = loop_genericIndex i0 s0
  where
    loop_genericIndex i !s = case next s of
      Done                   -> error "List.genericIndex: index too large."
      Skip    s'             -> expose s' $ loop_genericIndex i     s'
      Yield x s' | i == 0    -> x
                 | i > 0     -> expose s' $ loop_genericIndex (i-1) s'
                 | otherwise -> error "List.genericIndex: negative argument."
{-# INLINE [0] genericIndex #-}

-- can we pull the n > 0 test out and do it just once?
-- probably not since we don't know what n-1 does!!
-- can only specialise it for sane Integral instances :-(


genericSplitAt :: Integral i => i -> Stream a -> ([a], [a])
genericSplitAt n0 (Stream next s0) = loop_genericSplitAt n0 s0
  where
    loop_genericSplitAt 0 !s = ([], expose s $ unstream (Stream next s))
    loop_genericSplitAt n !s = case next s of
      Done            -> ([], [])
      Skip    s'      -> expose s $ loop_genericSplitAt n s'
      Yield x s'
        | n > 0       -> (x:xs', xs'')
        | otherwise   -> error "List.genericSplitAt: negative argument"
        where
          (xs', xs'') = expose s $ loop_genericSplitAt (n-1) s'
{-# INLINE [0] genericSplitAt #-}

{-
-- No need:
genericReplicate        -- :: Integral i => i -> a -> Stream a
-}

-- ---------------------------------------------------------------------
-- Enum

{-
enumFromToNum :: (Ord a, Num a) => a -> a -> Stream a
enumFromToNum x y = Stream next (L x)
  where
    {-# INLINE next #-}
    next (L !n)
        | n > y     = Done
        | otherwise = Yield n (L (n+1))
{-# INLINE [0] enumFromToNum #-}
-}

enumFromToInt :: Int -> Int -> Stream Int
enumFromToInt x y = Stream next (L x)
  where
    {-# INLINE next #-}
    next (L !n)
        | n > y     = Done
        | otherwise = Yield n (L (n+1))
{-# INLINE [0] enumFromToInt #-}

enumDeltaInteger :: Integer -> Integer -> Stream Integer
enumDeltaInteger a d = Stream next (L a)
  where
    {-# INLINE next #-}
    next (L !x) = Yield x (L (x+d))
{-# INLINE [0] enumDeltaInteger #-}

enumFromToChar :: Char -> Char -> Stream Char
enumFromToChar x y = Stream next (L (ord x))
  where
    m = ord y

    {-# INLINE next #-}
    next (L !n)
        | n > m     = Done
        | otherwise = Yield (chr n) (L (n+1))
{-# INLINE [0] enumFromToChar #-}

-- ---------------------------------------------------------------------
-- Monadic stuff

-- Most monadic list functions can be defined in terms of foldr so don't
-- need explicit stream implementations. The one exception is foldM:
--

foldM :: Monad m => (b -> a -> m b) -> b -> Stream a -> m b
foldM f z0 (Stream next s0) = loop_foldl z0 s0
  where
    loop_foldl z !s = case next s of
      Done       -> Monad.return z
      Skip    s' -> expose s' $                  loop_foldl z  s'
      Yield x s' -> expose s' $ f z x >>= \z' -> loop_foldl z' s'
{-# INLINE [0] foldM #-}

foldM_ :: Monad m => (b -> a -> m b) -> b -> Stream a -> m ()
foldM_ f z0 (Stream next s0) = loop_foldl z0 s0
  where
    loop_foldl z !s = case next s of
      Done       -> Monad.return ()
      Skip    s' -> expose s' $                  loop_foldl z  s'
      Yield x s' -> expose s' $ f z x >>= \z' -> loop_foldl z' s'
{-# INLINE [0] foldM_ #-}


-- ---------------------------------------------------------------------
-- List comprehension desugaring

return :: a -> Stream a
return e = Stream next S1
  where
    {-# INLINE next #-}
    next S1 = Yield e S2
    next S2 = Done
{-# INLINE [0] return #-}

guard :: Bool -> Stream a -> Stream a
guard b (Stream next0 s0) = Stream next (S1 :!: s0)
  where
    {-# INLINE next #-}
    next (S1 :!: s) = if b then Skip (S2 :!: s) else Done
    next (S2 :!: s) = case next0 s of
      Done       -> Done
      Skip    s' -> Skip    (S2 :!: s')
      Yield x s' -> Yield x (S2 :!: s')
{-# INLINE [0] guard #-}

bind :: (a -> Bool) -> (a -> Stream b) -> Stream a -> Stream b
bind b f (Stream next0 s0) = Stream next (s0 :!: Nothing)
  where
    {-# INLINE next #-}
    next (s :!: Nothing) = case next0 s of
      Done          -> Done
      Skip    s'    -> Skip    (s' :!: Nothing)
      Yield x s' 
        | b x       -> Skip (s' :!: Just (f x))
        | otherwise -> Skip (s' :!: Nothing)

    next (s :!: Just (Stream next1 s1)) = case next1 s1 of
      Done        -> Skip    (s :!: Nothing)
      Skip    s1' -> Skip    (s :!: Just (Stream next1 s1'))
      Yield x s1' -> Yield x (s :!: Just (Stream next1 s1'))
{-# INLINE [0] bind #-}

mapFilter :: (a -> Bool) -> (a -> b) -> Stream a -> Stream b
mapFilter b f (Stream next0 s0) = Stream next s0
  where
    {-# INLINE next #-}
    next s = case next0 s of
      Done          -> Done
      Skip    s'    -> Skip        s'
      Yield x s'
        | b x       -> Yield (f x) s'
        | otherwise -> Skip        s'
{-# INLINE [0] mapFilter #-}

declare :: (a -> Stream b) -> a -> Stream b
declare f bs = Stream next (f bs)
  where
    {-# INLINE next #-}
    next (Stream next0 s) = case next0 s of
      Done       -> Done
      Skip    s' -> Skip    (Stream next0 s')
      Yield x s' -> Yield x (Stream next0 s')
{-# INLINE [0] declare #-}

-- ---------------------------------------------------------------------
-- Internal utilities

-- Common up near identical calls to `error' to reduce the number
-- constant strings created when compiled:
errorEmptyStream :: String -> a
errorEmptyStream fun = moduleError fun "empty list"
{-# NOINLINE errorEmptyStream #-}

moduleError :: String -> String -> a
moduleError fun msg = error ("List." ++ fun ++ ':':' ':msg)
{-# NOINLINE moduleError #-}

#endif
