{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash    #-}
-- |
-- Module      : Data.List.Stream
-- Copyright   : (c) Duncan Coutts 2007
--               (c) Don Stewart   2007-2013
-- License     : BSD-style
-- Maintainer  : dons00@gmail.com
-- Stability   : experimental
-- Portability : portable
--
-- A reimplementation of the standard Haskell list library to take advantage of
-- stream fusion, and new GHC optimisations. The fusion mechanism is
-- based on stream fusion for sequences. Described in: 
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
-- This library is a drop in replacement for "Data.List".
--
module Data.List.Stream (

    -- $fusion_intro

    -- * Basic interface
    (++),                   -- :: [a] -> [a] -> [a]
    head,                   -- :: [a] -> a
    last,                   -- :: [a] -> a
    tail,                   -- :: [a] -> [a]
    init,                   -- :: [a] -> [a]
    null,                   -- :: [a] -> Bool
    length,                 -- :: [a] -> Int

    -- * List transformations
    map,                    -- :: (a -> b) -> [a] -> [b]
    reverse,                -- :: [a] -> [a]
    intersperse,            -- :: a -> [a] -> [a]
    intercalate,            -- :: [a] -> [[a]] -> [a]
    transpose,              -- :: [[a]] -> [[a]]

    -- * Reducing lists (folds)
    foldl,                  -- :: (a -> b -> a) -> a -> [b] -> a
    foldl',                 -- :: (a -> b -> a) -> a -> [b] -> a
    foldl1,                 -- :: (a -> a -> a) -> [a] -> a
    foldl1',                -- :: (a -> a -> a) -> [a] -> a
    foldr,                  -- :: (a -> b -> b) -> b -> [a] -> b
    foldr1,                 -- :: (a -> a -> a) -> [a] -> a

    -- ** Special folds
    concat,                 -- :: [[a]] -> [a]
    concatMap,              -- :: (a -> [b]) -> [a] -> [b]
    and,                    -- :: [Bool] -> Bool
    or,                     -- :: [Bool] -> Bool
    any,                    -- :: (a -> Bool) -> [a] -> Bool
    all,                    -- :: (a -> Bool) -> [a] -> Bool
    sum,                    -- :: Num a => [a] -> a
    product,                -- :: Num a => [a] -> a
    maximum,                -- :: Ord a => [a] -> a
    minimum,                -- :: Ord a => [a] -> a

    -- * Building lists
    -- ** Scans
    scanl,                  -- :: (a -> b -> a) -> a -> [b] -> [a]
    scanl1,                 -- :: (a -> a -> a) -> [a] -> [a]
    scanr,                  -- :: (a -> b -> b) -> b -> [a] -> [b]
    scanr1,                 -- :: (a -> a -> a) -> [a] -> [a]

    -- ** Accumulating maps
    mapAccumL,              -- :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
    mapAccumR,              -- :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])

    -- ** Infinite lists
    iterate,                -- :: (a -> a) -> a -> [a]
    repeat,                 -- :: a -> [a]
    replicate,              -- :: Int -> a -> [a]
    cycle,                  -- :: [a] -> [a]

    -- ** Unfolding
    unfoldr,                -- :: (b -> Maybe (a, b)) -> b -> [a]

    -- * Sublists
    -- ** Extracting sublists
    take,                   -- :: Int -> [a] -> [a]
    drop,                   -- :: Int -> [a] -> [a]
    splitAt,                -- :: Int -> [a] -> ([a], [a])
    takeWhile,              -- :: (a -> Bool) -> [a] -> [a]
    dropWhile,              -- :: (a -> Bool) -> [a] -> [a]
    span,                   -- :: (a -> Bool) -> [a] -> ([a], [a])
    break,                  -- :: (a -> Bool) -> [a] -> ([a], [a])
    group,                  -- :: Eq a => [a] -> [[a]]
    inits,                  -- :: [a] -> [[a]]
    tails,                  -- :: [a] -> [[a]]

    -- * Predicates
    isPrefixOf,             -- :: Eq a => [a] -> [a] -> Bool
    isSuffixOf,             -- :: Eq a => [a] -> [a] -> Bool
    isInfixOf,              -- :: Eq a => [a] -> [a] -> Bool

    -- * Searching lists
    -- ** Searching by equality
    elem,                   -- :: Eq a => a -> [a] -> Bool
    notElem,                -- :: Eq a => a -> [a] -> Bool
    lookup,                 -- :: Eq a => a -> [(a, b)] -> Maybe b

    -- ** Searching with a predicate
    find,                   -- :: (a -> Bool) -> [a] -> Maybe a
    filter,                 -- :: (a -> Bool) -> [a] -> [a]
    partition,              -- :: (a -> Bool) -> [a] -> ([a], [a])

    -- * Indexing lists

    -- | These functions treat a list @xs@ as a indexed collection,
    -- with indices ranging from 0 to @'length' xs - 1@.
    (!!),                   -- :: [a] -> Int -> a
    elemIndex,              -- :: Eq a => a -> [a] -> Maybe Int
    elemIndices,            -- :: Eq a => a -> [a] -> [Int]
    findIndex,              -- :: (a -> Bool) -> [a] -> Maybe Int
    findIndices,            -- :: (a -> Bool) -> [a] -> [Int]

    -- * Zipping and unzipping lists
    zip,                    -- :: [a] -> [b] -> [(a, b)]
    zip3,                   -- :: [a] -> [b] -> [c] -> [(a, b, c)]
    zip4,
    zip5,
    zip6,
    zip7,

    -- | The zipWith family generalises the zip family by zipping with the
    -- function given as the first argument, instead of a tupling function.
    zipWith,                -- :: (a -> b -> c) -> [a] -> [b] -> [c]
    zipWith3,               -- :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
    zipWith4,
    zipWith5,
    zipWith6,
    zipWith7,

    unzip,                  -- :: [(a, b)] -> ([a], [b])
    unzip3,                 -- :: [(a, b, c)] -> ([a], [b], [c])
    unzip4,
    unzip5,
    unzip6,
    unzip7,

    -- * Special lists
    -- ** Functions on strings
    lines,                  -- :: String -> [String]
    words,                  -- :: String -> [String]
    unlines,                -- :: [String] -> String
    unwords,                -- :: [String] -> String

    -- ** \"Set\" operations
    nub,                    -- :: Eq a => [a] -> [a]
    delete,                 -- :: Eq a => a -> [a] -> [a]
    (\\),                   -- :: Eq a => [a] -> [a] -> [a]
    union,                  -- :: Eq a => [a] -> [a] -> [a]
    intersect,              -- :: Eq a => [a] -> [a] -> [a]

    -- ** Ordered lists 
    sort,                   -- :: Ord a => [a] -> [a]
    insert,                 -- :: Ord a => a -> [a] -> [a]

    -- * Generalized functions
    -- ** The \"By\" operations

    -- | By convention, overloaded functions have a non-overloaded
    -- counterpart whose name is suffixed with \`@By@\'.
    --
    -- It is often convenient to use these functions together with
    -- 'Data.Function.on', for instance @'sortBy' ('compare'
    -- \`on\` 'fst')@.

    -- *** User-supplied equality (replacing an Eq context)

    -- | The predicate is assumed to define an equivalence.
    nubBy,                  -- :: (a -> a -> Bool) -> [a] -> [a]
    deleteBy,               -- :: (a -> a -> Bool) -> a -> [a] -> [a]
    deleteFirstsBy,         -- :: (a -> a -> Bool) -> [a] -> [a] -> [a]
    unionBy,                -- :: (a -> a -> Bool) -> [a] -> [a] -> [a]
    intersectBy,            -- :: (a -> a -> Bool) -> [a] -> [a] -> [a]
    groupBy,                -- :: (a -> a -> Bool) -> [a] -> [[a]]

    -- *** User-supplied comparison (replacing an Ord context)
    sortBy,                 -- :: (a -> a -> Ordering) -> [a] -> [a]
    insertBy,               -- :: (a -> a -> Ordering) -> a -> [a] -> [a]
    maximumBy,              -- :: (a -> a -> Ordering) -> [a] -> a
    minimumBy,              -- :: (a -> a -> Ordering) -> [a] -> a

    -- * The \"generic\" operations
    -- | The prefix \`@generic@\' indicates an overloaded function that
    -- is a generalized version of a "Prelude" function.
    genericLength,          -- :: Num i => [b] -> i
    genericTake,            -- :: Integral i => i -> [a] -> [a]
    genericDrop,            -- :: Integral i => i -> [a] -> [a]
    genericSplitAt,         -- :: Integral i => i -> [a] -> ([a], [a])
    genericIndex,           -- :: Integral a => [b] -> a -> b
    genericReplicate,       -- :: Integral i => i -> a -> [a]

    -- helper for GHC.List
    errorEmptyList          -- :: String -> a

  ) where

#ifndef EXTERNAL_PACKAGE

import {-# SOURCE #-} GHC.Err      ( error )
import {-# SOURCE #-} GHC.Real     (Integral)
import {-# SOURCE #-} GHC.Num      (Num(..))
import {-# SOURCE #-} GHC.Unicode  (isSpace)

import GHC.Base (Int, Eq(..), Ord(..), Ordering(..),
                 Bool(..), not, Ordering(..),
                 seq, otherwise, flip,
                 Monad(..),
                 Char, String,
                 Int(I#), Int#, (+#),
                 -- we just reuse these:
                 foldr, (++), map
                )

import Data.Maybe   (Maybe(..))

#else

import GHC.Exts (Int(I#), Int#, (+#))
import Prelude (Int,
                Integral,
                Num(..), Eq(..), Ord(..), Ordering(..),
                Bool(..), not, Maybe(..), Char, String,
                error, seq, otherwise, flip)

import Data.Char (isSpace)

#endif


import qualified Data.Stream as Stream
import Data.Stream (stream ,unstream)

-- -----------------------------------------------------------------------------

#ifdef EXTERNAL_PACKAGE
infixr 5 ++
#endif

infix  5 \\ -- comment to fool cpp
infixl 9 !!
infix  4 `elem`, `notElem`

-- -----------------------------------------------------------------------------

-- $fusion_intro
--
-- The functions in this library marked with /fusion/ are
-- (transparently) rewritten by the compiler to stream functions, using
-- the fusion framework described in /Rewriting Haskell Strings/.
--
-- For example:
--
-- > map f xs
-- 
-- is transformed via rewrite rules to:
--
-- > (unstream . mapS f . stream) xs
--
-- The 'unstream' and 'stream' functions identify the allocation points
-- for each function.
--
-- When two or more fusible functions are in close proximity (i.e.
-- directly composed, or with only intermediate lets and cases), the
-- fusion rule will fire, removing the intermediate structures.
--
-- Consider:
--
-- > map f . map g
-- 
-- The rewrite engine will transform this code to:
--
-- > unstream . mapS f . stream . unstream . mapS g . stream
--
-- The fusion rule will then fire:
--
-- > unstream . mapS f . mapS g . stream
--
-- Removing the intermeidate list that is allocated. The compiler then
-- optimises the result.
--
-- Functions that fail to fuse are not left in stream form. In the final
-- simplifier phase any remaining unfused functions of the form:
--
-- > unstream . g . stream
--
-- Will be transformed back to their original list implementation.
--

--
-- Notes on simplifer phasing
--
-- * api functions should be rewritten to fusible forms as soon as possble
-- * This implies a NOINLINE [1] on the top level functions, so if ghc wants
--      to inline them they'll only have their bodies inlined at the end.
-- * These rewrite rules can then fire in any but the last phase:
--      "++ -> fusible" [~1] forall xs ys.
-- * Finally, if we reach the final phase, rewrite back to best effort [a] forms:
--      "++ -> unfused" [1] forall xs ys.
-- * And then inline the result.
--
-- If fusion occurs though, hang on to those 'stream' and 'unstream' pairs:
--  {-# INLINE [0] unstream #-} -- hmm?
--
-- Todo: notes on the phasing of Streams
--

-- -----------------------------------------------------------------------------
-- Fusion for the constructors:

--
-- We do not enable fusion for (:), as it leads to a massive massive
-- slow down in compilation time.
--
{- RULES
"(:) -> fusible" [~1] forall x xs.
    x : xs = unstream (Stream.cons x (stream xs))
"(:) -> unfused" [1] forall x xs.
    unstream (Stream.cons x (stream xs)) = x : xs
  -}

-- -----------------------------------------------------------------------------
-- Basic interface

-- | /O(n)/, /fusion/. Append two lists, i.e.,
--
-- > [x1, ..., xm] ++ [y1, ..., yn] == [x1, ..., xm, y1, ..., yn]
-- > [x1, ..., xm] ++ [y1, ...] == [x1, ..., xm, y1, ...]
--
-- If the first list is not finite, the result is the first list.
-- The spine of the first list argument must be copied.

#ifdef EXTERNAL_PACKAGE
(++) :: [a] -> [a] -> [a]
(++) []     ys = ys
(++) (x:xs) ys = x : xs ++ ys
{-# NOINLINE [1] (++) #-}
#endif

-- NOTE: This is quite subtle as we do not want to copy the last list in
--
-- xs1 ++ xs2 ++ ... ++ xsn
--
-- Indeed, we don't really want to fuse the above at all unless at least 
-- one of the arguments has the form (unstream s) or the result of the
-- concatenation is streamed. The rules below do precisely that. Note they
-- really fuse instead of just rewriting things into a fusible form so there
-- is no need to rewrite back.

{-# RULES
"++ -> fused on 1st arg" [~1] forall xs ys.
    unstream xs ++ ys = Stream.append1 xs ys
"++ -> fused on 2nd arg" [~1] forall xs ys.
    Stream.append1 xs (unstream ys) = unstream (Stream.append xs ys)
"++ -> fused (1)" [~1] forall xs ys.
    stream (xs ++ ys) = Stream.append (stream xs) (stream ys)
"++ -> fused (2)" [~1] forall xs ys.
    stream (Stream.append1 xs ys) = Stream.append xs (stream ys)

"++ -> 1st arg empty" forall xs.
    [] ++ xs = xs
"++ -> 2nd arg empty" forall xs.
    xs ++ [] = xs
"++ / :" forall x xs ys.
    (x:xs) ++ ys = x : (xs ++ ys)
 #-}

-- | /O(1)/, /fusion/. Extract the first element of a list, which must be
-- non-empty.
head :: [a] -> a
head (x:_) = x
head []    = errorEmptyList "head"
{-# NOINLINE [1] head #-}

{-# RULES
"head -> fusible" [~1] forall xs.
    head xs = Stream.head (stream xs)
--"head -> unfused" [1] forall xs.
--    Stream.head (stream xs) = head xs
  #-}

-- | /O(n)/, /fusion/. Extract the last element of a list, which must be finite
-- and non-empty.
last :: [a] -> a
last []     = errorEmptyList "last"
last (x:xs) = last' x xs
  where
    last' y []     = y
    last' _ (y:ys) = last' y ys
{-# NOINLINE [1] last #-}

{-# RULES
"last -> fusible" [~1] forall xs.
    last xs = Stream.last (stream xs)
--"last -> unfused" [1] forall xs.
--    Stream.last (stream xs) = last xs
  #-}

-- | /O(1)/, /fusion/. Extract the elements after the head of a list, which
-- must be non-empty.
tail :: [a] -> [a]
tail (_:xs) = xs
tail []     = errorEmptyList "tail"
{-# NOINLINE [1] tail #-}

{-# RULES
"tail -> fusible" [~1] forall xs.
    tail xs = unstream (Stream.tail (stream xs))
--"tail -> unfused" [1] forall xs.
--    unstream (Stream.tail (stream xs)) = tail xs
  #-}

-- | /O(n)/, /fusion/. Return all the elements of a list except the last one.
-- The list must be finite and non-empty.
init :: [a] -> [a]
init []     = errorEmptyList "init"
init (x:xs) = init' x xs
  where
    init' _ []     = []
    init' y (z:zs) = y : init' z zs
{-# NOINLINE [1] init #-}

{-# RULES
"init -> fusible" [~1] forall xs.
    init xs = unstream (Stream.init (stream xs))
--"init -> unfused" [1] forall xs.
--    unstream (Stream.init (stream xs)) = init xs
  #-}

-- | /O(1)/, /fusion/. Test whether a list is empty.
null :: [a] -> Bool
null []    = True
null (_:_) = False
{-# NOINLINE [1] null #-}

{-# RULES
"null -> fusible" [~1] forall xs.
    null xs = Stream.null (stream xs)
--"null -> unfused" [1] forall xs.
--    Stream.null (stream xs) = null xs
  #-}

-- | /O(n)/, /fusion/. 'length' returns the length of a finite list as an 'Int'.
-- It is an instance of the more general 'Data.List.genericLength',
-- the result type of which may be any kind of number.
length :: [a] -> Int
length xs0 = len xs0 0#
#ifndef __HADDOCK__
  where
    len :: [a] -> Int# -> Int
    len []     a# = I# a#
    len (_:xs) a# = len xs (a# +# 1#)
#endif
{-# NOINLINE [1] length #-}

{-# RULES
"length -> fusible" [~1] forall xs.
    length xs = Stream.length (stream xs)
--"length -> unfused" [1] forall xs.
--    Stream.length (stream xs) = length xs
  #-}

-- ---------------------------------------------------------------------
-- List transformations

-- | /O(n)/, /fusion/. 'map' @f xs@ is the list obtained by applying @f@ to each element
-- of @xs@, i.e.,
--
-- > map f [x1, x2, ..., xn] == [f x1, f x2, ..., f xn]
-- > map f [x1, x2, ...] == [f x1, f x2, ...]
--
-- Properties:
--
-- > map f . map g         = map (f . g)
-- > map f (repeat x)      = repeat (f x)
-- > map f (replicate n x) = replicate n (f x)

#ifdef EXTERNAL_PACKAGE
map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs
{-# NOINLINE [1] map #-}
#endif

{-# RULES
"map -> fusible" [~1] forall f xs.
    map f xs = unstream (Stream.map f (stream xs))
--"map -> unfused" [1] forall f xs.
--    unstream (Stream.map f (stream xs)) = map f xs
  #-}

-- | /O(n)/, /fusion/. 'reverse' @xs@ returns the elements of @xs@ in reverse order.
-- @xs@ must be finite. Will fuse as a consumer only.
reverse :: [a] -> [a]
reverse = foldl' (flip (:)) []
{-# INLINE reverse #-}

{-
reverse l = rev l []
  where
    rev []     a = a
    rev (x:xs) a = rev xs (x:a)
-}

{-
--TODO: I'm sure there are some cunning things we can do with optimising
-- reverse. Of course if we try and fuse we may need to still force the
-- sping of the list: eg reverse . reverse = forceSpine

forceSpine :: [a] -> [a]
forceSpine xs = forceSpine' xs `seq` xs
{-# INLINE forceSpine #-}

-- The idea of this slightly odd construction is that we inline the above form
-- and in the context we may then be able to use xs directly and just keep
-- around the fact that xs must be forced at some point. Remember, seq does not
-- imply any evaluation order.

forceSpine' :: [a] -> ()
forceSpine' []      = ()
forceSpine' (_:xs') = forceSpine' xs'
{-# NOINLINE forceSpine' #-}
-}

-- | /O(n)/, /fusion/. The 'intersperse' function takes an element and a list and
-- \`intersperses\' that element between the elements of the list.
-- For example,
--
-- > intersperse ',' "abcde" == "a,b,c,d,e"
--
intersperse :: a -> [a] -> [a]
intersperse _   []       = []
intersperse sep (x0:xs0) = x0 : go xs0
  where
    go []     = []
    go (x:xs) = sep : x : go xs
{-# NOINLINE [1] intersperse #-}

{- RULES
"intersperse -> fusible" [~1] forall x xs.
    intersperse x xs = unstream (Stream.intersperse x (stream xs))
"intersperse -> unfused" [1] forall x xs.
    unstream (Stream.intersperse x (stream xs)) = intersperse x xs
  -}

-- | /O(n)/, /fusion/. 'intercalate' @xs xss@ is equivalent to @('concat' ('intersperse' xs xss))@.
-- It inserts the list @xs@ in between the lists in @xss@ and concatenates the
-- result.
--
-- > intercalate = concat . intersperse
--
intercalate :: [a] -> [[a]] -> [a]
intercalate sep xss = go (intersperse sep xss)
  where
    go []     = []
    go (y:ys) = y ++ go ys
{-# NOINLINE [1] intercalate #-}

{-
intercalate _   []         = []
intercalate sep (xs0:xss0) = go xs0 xss0
  where
    go []     xss = to xss
    go (x:xs) xss = x : go xs xss

    to []       = []
    to (xs:xss) = go' sep xs xss

    go' []     xs xss = go xs xss
    go' (s:ss) xs xss = s : go' ss xs xss
{-# NOINLINE [1] intercalate #-}
-}

-- fusion rule based on:
--      intercalate = concat . intersperse
--
{- RULES
"intercalate -> fusible" [~1] forall x xs.
    intercalate x xs = Stream.concat (Stream.intersperse x (stream xs))
"intercalate -> unfused" [1] forall x xs.
    Stream.concat (Stream.intersperse x (stream xs)) = intercalate x xs
  -}

-- | The 'transpose' function transposes the rows and columns of its argument.
-- For example,
--
-- > transpose [[1,2,3],[4,5,6]] == [[1,4],[2,5],[3,6]]
--
transpose :: [[a]] -> [[a]]
transpose []             = []
transpose ([]     : xss) = transpose xss
transpose ((x:xs) : xss) = (x : [h | (h:_t) <- xss])
                         : transpose (xs : [ t | (_h:t) <- xss])

-- TODO fuse

-- ---------------------------------------------------------------------
-- Reducing lists (folds)

-- | /O(n)/, /fusion/. 'foldl', applied to a binary operator, a starting value (typically
-- the left-identity of the operator), and a list, reduces the list
-- using the binary operator, from left to right:
--
-- > foldl f z [x1, x2, ..., xn] == (...((z `f` x1) `f` x2) `f`...) `f` xn
--
-- The list must be finite.
--
foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f z0 xs0 = go z0 xs0
  where
    go z []     = z
    go z (x:xs) = go (f z x) xs
{-# INLINE [1] foldl #-}

{-# RULES
"foldl -> fusible"  [~1] forall f z xs.
    foldl f z xs = Stream.foldl f z (stream xs)
--"foldl -> unfused" [1] forall f z xs.
--    Stream.foldl f z (stream xs) = foldl f z xs
  #-}

-- | /O(n)/, /fusion/. A strict version of 'foldl'.
foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f z0 xs0 = go z0 xs0
#ifndef __HADDOCK__
  where
    go !z []     = z
    go !z (x:xs) = go (f z x) xs
#endif
{-# INLINE [1] foldl' #-}

{-# RULES
"foldl' -> fusible"  [~1] forall f z xs.
    foldl' f z xs = Stream.foldl' f z (stream xs)
--"foldl' -> unfused" [1] forall f z xs.
--    Stream.foldl' f z (stream xs) = foldl' f z xs
  #-}

-- | /O(n)/, /fusion/. 'foldl1' is a variant of 'foldl' that has no starting value argument,
-- and thus must be applied to non-empty lists.
foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 _ []       = errorEmptyList "foldl1"
foldl1 f (x0:xs0) = go x0 xs0
  where
    go z []     = z
    go z (x:xs) = go (f z x) xs
{-# INLINE [1] foldl1 #-}

{-# RULES
"foldl1 -> fusible"  [~1] forall f xs.
    foldl1 f xs = Stream.foldl1 f (stream xs)
--"foldl1 -> unfused" [1] forall f xs.
--    Stream.foldl1 f (stream xs) = foldl1 f xs
  #-}

-- | /O(n)/, /fusion/. A strict version of 'foldl1'
foldl1' :: (a -> a -> a) -> [a] -> a
foldl1' _ []       = errorEmptyList "foldl1'"
foldl1' f (x0:xs0) = go x0 xs0
#ifndef __HADDOCK__
  where
    go !z []     = z
    go !z (x:xs) = go (f z x) xs
#endif
{-# INLINE [1] foldl1' #-}

{-# RULES
"foldl1' -> fusible"  [~1] forall f xs.
    foldl1' f xs = Stream.foldl1' f (stream xs)
--"foldl1 -> unfused" [1] forall f xs.
--    Stream.foldl1' f (stream xs) = foldl1' f xs
  #-}

-- | /O(n)/, /fusion/. 'foldr', applied to a binary operator, a starting value (typically
-- the right-identity of the operator), and a list, reduces the list
-- using the binary operator, from right to left:
--
-- > foldr f z [x1, x2, ..., xn] == x1 `f` (x2 `f` ... (xn `f` z)...)

#ifdef EXTERNAL_PACKAGE
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr k z xs = go xs
  where
    go []     = z
    go (y:ys) = y `k` go ys
{-# INLINE [0] foldr #-}
#endif

{-# RULES
"foldr -> fusible"  [~1] forall f z xs.
    foldr f z xs = Stream.foldr f z (stream xs)
--"foldr -> unfused" [1] forall f z xs.
--    Stream.foldr f z (stream xs) = foldr f z xs
  #-}

-- | /O(n)/, /fusion/. 'foldr1' is a variant of 'foldr' that has no starting value argument,
-- and thus must be applied to non-empty lists.
foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 _ []          = errorEmptyList "foldr1"
foldr1 k (x0:xs0)    = go x0 xs0
  where go x []      = x
        go x (x':xs) = k x (go x' xs)
{-# INLINE [1] foldr1 #-}

{-# RULES
"foldr1 -> fusible"  [~1] forall f xs.
    foldr1 f xs = Stream.foldr1 f (stream xs)
--"foldr1 -> unfused" [1] forall f xs.
--    Stream.foldr1 f (stream xs) = foldr1 f xs
  #-}

-- ---------------------------------------------------------------------
-- Special folds

-- | /O(n)/, /fusion/. Concatenate a list of lists.
concat :: [[a]] -> [a]
concat xss0 = to xss0
  where go []     xss = to xss
        go (x:xs) xss = x : go xs xss

        to []       = []
        to (xs:xss) = go xs xss -- hmm, this is slower than the old concat?
{-# NOINLINE [1] concat #-}

--
-- fuse via concatMap, as the Stream (Stream a) is too hard to construct
-- 
-- or via foldr (++) ?
--
{-# RULES
"concat -> fused"  [~1] forall xs.
    concat xs = Stream.concat (stream xs)
--"concat -> unfused" [1] forall xs.
--    Stream.concat (stream xs) = concat xs
  #-}

-- | /O(n)/, /fusion/. Map a function over a list and concatenate the results.
concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f = foldr (\x y -> f x ++ y) [] -- at least it will fuse.
{-# INLINE concatMap #-}

{-
concatMap f as0 = to as0
  where
    go []     as = to as
    go (b:bs) as = b : go bs as

    to []     = []
    to (a:as) = go (f a) as
{-# NOINLINE [1] concatMap #-}
-}
{- RULES
"concatMap -> fusible"  [~1] forall f xs.
    concatMap f xs = Stream.concatMap f (stream xs)
"concatMap -> unfused"  [1] forall f xs.
    Stream.concatMap f (stream xs) = concatMap f xs
  -}

-- | /O(n)/, /fusion/. 'and' returns the conjunction of a Boolean list.  For the result to be
-- 'True', the list must be finite; 'False', however, results from a 'False'
-- value at a finite index of a finite or infinite list.
--
and :: [Bool] -> Bool
and []         = True
and (False:_ ) = False
and (_    :xs) = and xs
{-# NOINLINE [1] and #-}

{-# RULES
"and -> fused"  [~1] forall xs.
    and xs = Stream.and (stream xs)
--"and -> unfused" [1] forall xs.
--    Stream.and (stream xs) = and xs
  #-}

-- | /O(n)/, /fusion/. 'or' returns the disjunction of a Boolean list.  For the result to be
-- 'False', the list must be finite; 'True', however, results from a 'True'
-- value at a finite index of a finite or infinite list.
or :: [Bool] -> Bool
or []        = False
or (True:_ ) = True
or (_   :xs) = or xs
{-# NOINLINE [1] or #-}

{-# RULES
"or -> fused"  [~1] forall xs.
    or xs = Stream.or (stream xs)
--"or -> unfused" [1] forall xs.
--    Stream.or (stream xs) = or xs
  #-}

-- | /O(n)/, /fusion/. Applied to a predicate and a list, 'any' determines if any element
-- of the list satisfies the predicate.
any :: (a -> Bool) -> [a] -> Bool
any p xs0 = go xs0
  where go []     = False
        go (x:xs) = case p x of
                      True  -> True
                      False -> go xs
{-# NOINLINE [1] any #-}

--TODO: check if being lazy in p is a cost,
--      should we do [] as a special case and then strictly evaluate p?

{-# RULES
"any -> fusible"  [~1] forall f xs.
    any f xs = Stream.any f (stream xs)
--"any -> unfused" [1] forall f xs.
--    Stream.any f (stream xs) = any f xs
  #-}

-- | Applied to a predicate and a list, 'all' determines if all elements
-- of the list satisfy the predicate.
all :: (a -> Bool) -> [a] -> Bool
all p xs0 = go xs0
  where go []     = True
        go (x:xs) = case p x of
                      True  -> go xs
                      False -> False
{-# NOINLINE [1] all #-}

{-# RULES
"all -> fusible"  [~1] forall f xs.
    all f xs = Stream.all f (stream xs)
--"all -> unfused" [1] forall f xs.
--    Stream.all f (stream xs) = all f xs
  #-}

-- | /O(n)/, /fusion/. The 'sum' function computes the sum of a finite list of numbers.
sum :: Num a => [a] -> a
sum l = sum' l 0
#ifndef __HADDOCK__
  where
    sum' []     a = a
    sum' (x:xs) a = sum' xs (a+x)
#endif
{-# NOINLINE [1] sum #-}

sumInt :: [Int] -> Int
sumInt l = sum' l 0
#ifndef __HADDOCK__
  where
    sum' []      a = a
    sum' (x:xs) !a = sum' xs (a+x)
#endif
{-# NOINLINE [1] sumInt #-}

{-# RULES
"sum spec Int" sum = sumInt :: [Int] -> Int
  #-}

{-# RULES
"sum -> fusible"  [~1] forall xs.
    sum xs = Stream.sum (stream xs)
--"sum -> unfused" [1] forall xs.
--    Stream.sum (stream xs) = sum xs
  #-}

{-# RULES
"sumInt -> fusible"  [~1] forall (xs :: [Int]).
    sumInt xs = Stream.sum (stream xs)
--"sumInt -> unfused" [1] forall (xs :: [Int]).
--    Stream.sum (stream xs) = sumInt xs
  #-}

-- | /O(n)/,/fusion/. The 'product' function computes the product of a finite list of numbers.
product :: Num a => [a] -> a
product l = prod l 1
#ifndef __HADDOCK__
  where
    prod []     a = a
    prod (x:xs) a = prod xs (a*x)
#endif
{-# NOINLINE [1] product #-}

productInt :: [Int] -> Int
productInt l = product' l 0
#ifndef __HADDOCK__
  where
    product' []      a = a
    product' (x:xs) !a = product' xs (a*x)
#endif
{-# NOINLINE [1] productInt #-}

{-# RULES
"product spec Int" product = productInt :: [Int] -> Int
  #-}

{-# RULES
"product -> fused"  [~1] forall xs.
    product xs = Stream.product (stream xs)
--"product -> unfused" [1] forall xs.
--    Stream.product (stream xs) = product xs
  #-}

{-# RULES
"productInt -> fusible"  [~1] forall (xs :: [Int]).
    productInt xs = Stream.product (stream xs)
--"productInt -> unfused" [1] forall (xs :: [Int]).
--    Stream.product (stream xs) = productInt xs
  #-}

-- | /O(n)/,/fusion/. 'maximum' returns the maximum value from a list,
-- which must be non-empty, finite, and of an ordered type.
-- It is a special case of 'Data.List.maximumBy', which allows the
-- programmer to supply their own comparison function.
maximum :: Ord a => [a] -> a
maximum []       = errorEmptyList "maximum"
maximum xs       = foldl1 max xs
{-# NOINLINE [1] maximum #-}

{-# RULES
"maximum -> fused"  [~1] forall xs.
    maximum xs = Stream.maximum (stream xs)
--"maximum -> unfused" [1] forall xs.
--    Stream.maximum (stream xs) = maximum xs
  #-}

-- We can't make the overloaded version of maximum strict without
-- changing its semantics (max might not be strict), but we can for
-- the version specialised to 'Int'.

{-# RULES
  "maximumInt"     maximum = (strictMaximum :: [Int] -> Int);
  "maximumChar"    maximum = (strictMaximum :: [Char] -> Char)
  #-}

strictMaximum :: (Ord a) => [a] -> a
strictMaximum [] =  errorEmptyList "maximum"
strictMaximum xs =  foldl1' max xs
{-# NOINLINE [1] strictMaximum #-}

{-# RULES
"strictMaximum -> fused"  [~1] forall xs.
    strictMaximum xs = Stream.strictMaximum (stream xs)
--"strictMaximum -> unfused" [1] forall xs.
--    Stream.strictMaximum (stream xs) = strictMaximum xs
  #-}

-- | /O(n)/,/fusion/. 'minimum' returns the minimum value from a list,
-- which must be non-empty, finite, and of an ordered type.
-- It is a special case of 'Data.List.minimumBy', which allows the
-- programmer to supply their own comparison function.
minimum :: Ord a => [a] -> a
minimum [] = errorEmptyList "minimum"
minimum xs = foldl1 min xs
{-# NOINLINE [1] minimum #-}

{-# RULES
"minimum -> fused"  [~1] forall xs.
    minimum xs = Stream.minimum (stream xs)
--"minimum -> unfused" [1] forall xs.
--    Stream.minimum (stream xs) = minimum xs
  #-}

{-# RULES
  "minimumInt"     minimum = (strictMinimum :: [Int]  -> Int);
  "minimumChar"    minimum = (strictMinimum :: [Char] -> Char)
  #-}

strictMinimum :: (Ord a) => [a] -> a
strictMinimum [] = errorEmptyList "maximum"
strictMinimum xs = foldl1' min xs
{-# NOINLINE [1] strictMinimum #-}

{-# RULES
"strictMinimum -> fused"  [~1] forall xs.
    strictMinimum xs = Stream.strictMinimum (stream xs)
--"strictMinimum -> unfused" [1] forall xs.
--    Stream.strictMinimum (stream xs) = strictMinimum xs
  #-}

-- ---------------------------------------------------------------------
-- * Building lists
-- ** Scans

-- | /O(n)/, /fusion/. 'scanl' is similar to 'foldl', but returns a list of successive
-- reduced values from the left:
--
-- > scanl f z [x1, x2, ...] == [z, z `f` x1, (z `f` x1) `f` x2, ...]
--
-- Properties:
--
-- > last (scanl f z xs) == foldl f z x
--
scanl :: (a -> b -> a) -> a -> [b] -> [a]
scanl f q ls = q : case ls of
                      []   -> []
                      x:xs -> scanl f (f q x) xs
{-# INLINE [1] scanl #-}

{- or perhaps:
scanl f q xs0 = q : go q xs0
  where go q []     = []
        go q (x:xs) = let q' = f q x
                       in q' : go q' xs
-}

-- 
-- note: Haskell's 'scan' is a bit weird, as it always puts the initial
-- state as a prefix. this complicates the rules.
--

{-# RULES
"scanl -> fusible"  [~1] forall f z xs.
    scanl f z xs = unstream (Stream.scanl f z (Stream.snoc (stream xs) bottom))
--"scanl -> unfused" [1] forall f z xs.
--    unstream (Stream.scanl f z (Stream.snoc (stream xs) bottom)) = scanl f z xs
  #-}

-- | /O(n)/,/fusion/. 'scanl1' is a variant of 'scanl' that has no starting value argument:
--
-- > scanl1 f [x1, x2, ...] == [x1, x1 `f` x2, ...]
--
scanl1 :: (a -> a -> a) -> [a] -> [a]
scanl1 f (x:xs) = scanl f x xs
scanl1 _ []     = []
{-# INLINE [1] scanl1 #-}

{-# RULES
"scanl1 -> fusible"  [~1] forall f xs.
    scanl1 f xs = unstream (Stream.scanl1 f (Stream.snoc (stream xs) bottom))
--"scanl1 -> unfused" [1] forall f xs.
--    unstream (Stream.scanl1 f (Stream.snoc (stream xs) bottom)) = scanl1 f xs
  #-}


-- | /O(n)/. 'scanr' is the right-to-left dual of 'scanl'.
-- Properties:
--
-- > head (scanr f z xs) == foldr f z xs
--
scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr _ q0 []     = [q0]
scanr f q0 (x:xs) = f x q : qs
                    where qs@(q:_) = scanr f q0 xs
{-# INLINE [1] scanr #-}

{- RULES
"scanr -> fusible"  [~1] forall f z xs.
    scanr f z xs = unstream (Stream.scanr f z (Stream.cons bottom (stream xs)))
"scanr -> unfused" [1] forall f z xs.
    unstream (Stream.scanr f z (Stream.cons bottom (stream xs))) = scanr f z xs
  -}

-- | 'scanr1' is a variant of 'scanr' that has no starting value argument.
scanr1 :: (a -> a -> a) -> [a] -> [a]
scanr1 _ []     = []
scanr1 _ [x]    = [x]
scanr1 f (x:xs) = f x q : qs
                  where qs@(q:_) = scanr1 f xs

-- TODO fuse

-- ---------------------------------------------------------------------
-- ** Accumulating maps

-- | The 'mapAccumL' function behaves like a combination of 'map' and
-- 'foldl'; it applies a function to each element of a list, passing
-- an accumulating parameter from left to right, and returning a final
-- value of this accumulator together with the new list.
--
mapAccumL :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
mapAccumL _ s []     = (s, [])
mapAccumL f s (x:xs) = (s'',y:ys)
                       where (s', y ) = f s x
                             (s'',ys) = mapAccumL f s' xs

-- TODO fuse

-- | The 'mapAccumR' function behaves like a combination of 'map' and
-- 'foldr'; it applies a function to each element of a list, passing
-- an accumulating parameter from right to left, and returning a final
-- value of this accumulator together with the new list.
--
mapAccumR :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
mapAccumR _ s []     = (s, [])
mapAccumR f s (x:xs) = (s'', y:ys)
                       where (s'',y ) = f s' x
                             (s', ys) = mapAccumR f s xs

-- TODO fuse

------------------------------------------------------------------------
-- ** Infinite lists

-- | /fusion/. 'iterate' @f x@ returns an infinite list of repeated applications
-- of @f@ to @x@:
--
-- > iterate f x == [x, f x, f (f x), ...]
iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)
{-# NOINLINE [1] iterate #-}

{-# RULES
"iterate -> fusible" [~1] forall f x.
    iterate f x = unstream (Stream.iterate f x)
--"iterate -> unfused" [1] forall f x.
--    unstream (Stream.iterate f x) = iterate f x
  #-}

-- | /fusion/. 'repeat' @x@ is an infinite list, with @x@ the value of every element.
repeat :: a -> [a]
repeat x = xs where xs = x : xs
{-# INLINE [1] repeat #-}

{-# RULES
"repeat -> fusible" [~1] forall x.
    repeat x = unstream (Stream.repeat x)
--"repeat -> unfused" [1] forall x.
--    unstream (Stream.repeat x) = repeat x
  #-}

-- | /O(n)/, /fusion/. 'replicate' @n x@ is a list of length @n@ with @x@ the value of
-- every element.
-- It is an instance of the more general 'Data.List.genericReplicate',
-- in which @n@ may be of any integral type.
--
replicate :: Int -> a -> [a]
replicate n0 _ | n0 <= 0 = []
replicate n0 x           = go n0
  where
    go 0 = []
    go n = x : go (n-1)
{-# NOINLINE [1] replicate #-}

{-# RULES
"replicate -> fusible" [~1]
    replicate = \n x -> unstream (Stream.replicate n x)
--"replicate -> unfused" [1] forall n x.
--    unstream (Stream.replicate n x) = replicate n x
  #-}

-- | /fusion/. 'cycle' ties a finite list into a circular one, or equivalently,
-- the infinite repetition of the original list.  It is the identity
-- on infinite lists.
--
cycle :: [a] -> [a]
cycle [] = error "Prelude.cycle: empty list"
cycle xs0 = go xs0
  where
    go []     = go xs0
    go (x:xs) = x : go xs
{-# NOINLINE [1] cycle #-}

{-# RULES
"cycle -> fusible" [~1] forall xs.
    cycle xs = unstream (Stream.cycle (stream xs))
--"cycle -> unfused" [1] forall xs.
--    unstream (Stream.cycle (stream xs)) = cycle xs
  #-}

-- ---------------------------------------------------------------------
-- ** Unfolding

-- | /fusion/. The 'unfoldr' function is a \`dual\' to 'foldr': while 'foldr'
-- reduces a list to a summary value, 'unfoldr' builds a list from
-- a seed value.  The function takes the element and returns 'Nothing'
-- if it is done producing the list or returns 'Just' @(a,b)@, in which
-- case, @a@ is a prepended to the list and @b@ is used as the next
-- element in a recursive call.  For example,
--
-- > iterate f == unfoldr (\x -> Just (x, f x))
--
-- In some cases, 'unfoldr' can undo a 'foldr' operation:
--
-- > unfoldr f' (foldr f z xs) == xs
--
-- if the following holds:
--
-- > f' (f x y) = Just (x,y)
-- > f' z       = Nothing
--
-- A simple use of unfoldr:
--
-- > unfoldr (\b -> if b == 0 then Nothing else Just (b, b-1)) 10
-- >  [10,9,8,7,6,5,4,3,2,1]
--
unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f b0 = unfold b0
  where
    unfold b = case f b of
      Just (a,b') -> a : unfold b'
      Nothing     -> []
{-# INLINE [1] unfoldr #-}

{-# RULES
"unfoldr -> fusible" [~1] forall f x.
    unfoldr f x = unstream (Stream.unfoldr f x)
--"unfoldr -> unfused" [1] forall f x.
--    unstream (Stream.unfoldr f x) = unfoldr f x
  #-}

------------------------------------------------------------------------
-- * Sublists
-- ** Extracting sublists

-- | /O(n)/,/fusion/. 'take' @n@, applied to a list @xs@, returns the prefix of @xs@
-- of length @n@, or @xs@ itself if @n > 'length' xs@:
--
-- > take 5 "Hello World!" == "Hello"
-- > take 3 [1,2,3,4,5] == [1,2,3]
-- > take 3 [1,2] == [1,2]
-- > take 3 [] == []
-- > take (-1) [1,2] == []
-- > take 0 [1,2] == []
--
-- It is an instance of the more general 'Data.List.genericTake',
-- in which @n@ may be of any integral type.
--
take :: Int -> [a] -> [a]
take i _ | i <= 0 = []
take i ls = take' i ls
  where
    take' :: Int -> [a] -> [a]
    take' 0 _      = []
    take' _ []     = []
    take' n (x:xs) = x : take' (n-1) xs
{-# NOINLINE [1] take #-}

{-# RULES
"take -> fusible" [~1] forall n x.
    take n x = unstream (Stream.take n (stream x))
--"take -> unfused" [1] forall n x.
--    unstream (Stream.take n (stream x)) = take n x
  #-}

{-
take :: Int -> [a] -> [a]
take (I# n#) xs = takeUInt n# xs

takeUInt :: Int# -> [b] -> [b]
takeUInt n xs
    | n >=# 0#  =  take_unsafe_UInt n xs
    | otherwise =  []

take_unsafe_UInt :: Int# -> [b] -> [b]
take_unsafe_UInt 0#  _  = []
take_unsafe_UInt m   ls =
  case ls of
    []     -> []
    (x:xs) -> x : take_unsafe_UInt (m -# 1#) xs
-}

-- | /O(n)/,/fusion/. 'drop' @n xs@ returns the suffix of @xs@
-- after the first @n@ elements, or @[]@ if @n > 'length' xs@:
--
-- > drop 6 "Hello World!" == "World!"
-- > drop 3 [1,2,3,4,5] == [4,5]
-- > drop 3 [1,2] == []
-- > drop 3 [] == []
-- > drop (-1) [1,2] == [1,2]
-- > drop 0 [1,2] == [1,2]
--
-- It is an instance of the more general 'Data.List.genericDrop',
-- in which @n@ may be of any integral type.
--
drop :: Int -> [a] -> [a]
drop n ls
  | n < 0     = ls
  | otherwise = drop' n ls
  where
    drop' :: Int -> [a] -> [a]
    drop' 0 xs      = xs
    drop' _  xs@[]  = xs
    drop' m (_:xs)  = drop' (m-1) xs
{-# NOINLINE [1] drop #-}

{-# RULES
"drop -> fusible" [~1] forall n x.
    drop n x = unstream (Stream.drop n (stream x))
--"drop -> unfused" [1] forall n x.
--    unstream (Stream.drop n (stream x)) = drop n x
  #-}

-- | 'splitAt' @n xs@ returns a tuple where first element is @xs@ prefix of
-- length @n@ and second element is the remainder of the list:
--
-- > splitAt 6 "Hello World!" == ("Hello ","World!")
-- > splitAt 3 [1,2,3,4,5] == ([1,2,3],[4,5])
-- > splitAt 1 [1,2,3] == ([1],[2,3])
-- > splitAt 3 [1,2,3] == ([1,2,3],[])
-- > splitAt 4 [1,2,3] == ([1,2,3],[])
-- > splitAt 0 [1,2,3] == ([],[1,2,3])
-- > splitAt (-1) [1,2,3] == ([],[1,2,3])
--
-- It is equivalent to @('take' n xs, 'drop' n xs)@.
-- 'splitAt' is an instance of the more general 'Data.List.genericSplitAt',
-- in which @n@ may be of any integral type.
--
splitAt :: Int -> [a] -> ([a], [a])
splitAt n ls
  | n < 0     = ([], ls)
  | otherwise = splitAt' n ls
  where
    splitAt' :: Int -> [a] -> ([a], [a])
    splitAt' 0 xs     = ([], xs)
    splitAt' _  xs@[] = (xs, xs)
    splitAt' m (x:xs) = (x:xs', xs'')
      where
        (xs', xs'') = splitAt' (m-1) xs
{-# NOINLINE [1] splitAt #-}

{-
splitAt n xs | n <= 0 = ([], xs)
splitAt _ []          = ([], [])
splitAt n (x:xs)      = (x:xs', xs'')
  where
    (xs', xs'') = splitAt (n-1) xs
-}

{-# RULES
"splitAt -> fusible" [~1] forall n xs.
    splitAt n xs = Stream.splitAt n (stream xs)
--"splitAt -> unfused" [1] forall n xs.
--    Stream.splitAt n (stream xs) = splitAt n xs
  #-}

-- | /O(n)/,/fusion/. 'takeWhile', applied to a predicate @p@ and a list @xs@, returns the
-- longest prefix (possibly empty) of @xs@ of elements that satisfy @p@:
--
-- > takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
-- > takeWhile (< 9) [1,2,3] == [1,2,3]
-- > takeWhile (< 0) [1,2,3] == []
--
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ []    = []
takeWhile p xs0   = go xs0
  where
    go []         = []
    go (x:xs)
      | p x       = x : go xs
      | otherwise = []
{-# NOINLINE [1] takeWhile #-}

{-# RULES
"takeWhile -> fusible" [~1] forall f xs.
    takeWhile f xs = unstream (Stream.takeWhile f (stream xs))
--"takeWhile -> unfused" [1] forall f xs.
--    unstream (Stream.takeWhile f (stream xs)) = takeWhile f xs
  #-}

-- | /O(n)/,/fusion/. 'dropWhile' @p xs@ returns the suffix remaining after 'takeWhile' @p xs@:
--
-- > dropWhile (< 3) [1,2,3,4,5,1,2,3] == [3,4,5,1,2,3]
-- > dropWhile (< 9) [1,2,3] == []
-- > dropWhile (< 0) [1,2,3] == [1,2,3]
--
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ []    = []
dropWhile p xs0   = go xs0
  where
    go []         = []
    go xs@(x:xs')
      | p x       = go xs'
      | otherwise = xs
{-# NOINLINE [1] dropWhile #-}

{-# RULES
"dropWhile -> fusible" [~1] forall f xs.
    dropWhile f xs = unstream (Stream.dropWhile f (stream xs))
--"dropWhile -> unfused" [1] forall f xs.
--    unstream (Stream.dropWhile f (stream xs)) = dropWhile f xs
  #-}

-- | 'span', applied to a predicate @p@ and a list @xs@, returns a tuple where
-- first element is longest prefix (possibly empty) of @xs@ of elements that
-- satisfy @p@ and second element is the remainder of the list:
-- 
-- > span (< 3) [1,2,3,4,1,2,3,4] == ([1,2],[3,4,1,2,3,4])
-- > span (< 9) [1,2,3] == ([1,2,3],[])
-- > span (< 0) [1,2,3] == ([],[1,2,3])
-- 
-- 'span' @p xs@ is equivalent to @('takeWhile' p xs, 'dropWhile' p xs)@
span :: (a -> Bool) -> [a] -> ([a], [a])
span _ []         = ([], [])
span p xs0        = go xs0
  where
    go []         = ([], [])
    go xs@(x:xs')
      | p x       = let (ys,zs) = go xs'
                     in (x:ys,zs)
      | otherwise = ([],xs)

-- TODO fuse
-- Hmm, these do a lot of sharing, but is it worth it?

-- | 'break', applied to a predicate @p@ and a list @xs@, returns a tuple where
-- first element is longest prefix (possibly empty) of @xs@ of elements that
-- /do not satisfy/ @p@ and second element is the remainder of the list:
-- 
-- > break (> 3) [1,2,3,4,1,2,3,4] == ([1,2,3],[4,1,2,3,4])
-- > break (< 9) [1,2,3] == ([],[1,2,3])
-- > break (> 9) [1,2,3] == ([1,2,3],[])
--
-- 'break' @p@ is equivalent to @'span' ('not' . p)@.
--
break :: (a -> Bool) -> [a] -> ([a], [a])
break _ []        = ([], [])
break p xs0       = go xs0
  where
    go []         = ([], [])
    go xs@(x:xs')
      | p x       = ([],xs)
      | otherwise = let (ys,zs) = go xs'
                    in (x:ys,zs)

-- TODO fuse

-- | The 'group' function takes a list and returns a list of lists such
-- that the concatenation of the result is equal to the argument.  Moreover,
-- each sublist in the result contains only equal elements.  For example,
--
-- > group "Mississippi" = ["M","i","ss","i","ss","i","pp","i"]
--
-- It is a special case of 'groupBy', which allows the programmer to supply
-- their own equality test.
group :: Eq a => [a] -> [[a]]
group []     = []
group (x:xs) = (x:ys) : group zs
               where (ys,zs) = span (x ==) xs

-- TODO fuse

-- | The 'inits' function returns all initial segments of the argument,
-- shortest first.  For example,
--
-- > inits "abc" == ["","a","ab","abc"]
--
inits :: [a] -> [[a]]
inits []     = [] : []
inits (x:xs) = [] : map (x:) (inits xs)

-- TODO fuse

-- | The 'tails' function returns all final segments of the argument,
-- longest first.  For example,
--
-- > tails "abc" == ["abc", "bc", "c",""]
--
tails :: [a] -> [[a]]
tails []         = []  : []
tails xxs@(_:xs) = xxs : tails xs

-- TODO fuse

------------------------------------------------------------------------
-- * Predicates

-- | /O(n)/,/fusion/. The 'isPrefixOf' function takes two lists and
-- returns 'True' iff the first list is a prefix of the second.
--
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf [] _                      = True
isPrefixOf _  []                     = False
isPrefixOf (x:xs) (y:ys) | x == y    = isPrefixOf xs ys
                         | otherwise = False
{-# NOINLINE [1] isPrefixOf #-}

{-# RULES
"isPrefixOf -> fusible" [~1] forall xs ys.
    isPrefixOf xs ys = Stream.isPrefixOf (stream xs) (stream ys)
--"isPrefixOf -> unfused" [1]  forall xs ys.
--    Stream.isPrefixOf (stream xs) (stream ys) = isPrefixOf xs ys
  #-}

-- | The 'isSuffixOf' function takes two lists and returns 'True'
-- iff the first list is a suffix of the second.
-- Both lists must be finite.
isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf x y = reverse x `isPrefixOf` reverse y

-- TODO fuse

-- | The 'isInfixOf' function takes two lists and returns 'True'
-- iff the first list is contained, wholly and intact,
-- anywhere within the second.
--
-- Example:
--
-- > isInfixOf "Haskell" "I really like Haskell." -> True
-- > isInfixOf "Ial" "I really like Haskell." -> False
--
isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)

-- TODO fuse

-- ---------------------------------------------------------------------
-- * Searching lists
-- ** Searching by equality

-- | /O(n)/, /fusion/. 'elem' is the list membership predicate, usually written
-- in infix form, e.g., @x `elem` xs@.
--
elem :: Eq a => a -> [a] -> Bool
elem _ []     = False
elem x (y:ys)
  | x == y    = True
  | otherwise = elem x ys
{-# NOINLINE [1] elem #-}

{-# RULES
"elem -> fusible" [~1] forall x xs.
    elem x xs = Stream.elem x (stream xs)
--"elem -> unfused" [1] forall x xs.
--    Stream.elem x (stream xs) = elem x xs
  #-}

-- | /O(n)/, /fusion/. 'notElem' is the negation of 'elem'.
notElem :: Eq a => a -> [a] -> Bool
notElem x xs = not (elem x xs)
{-# INLINE notElem #-}

{- RULES
-- We don't provide an expicilty fusible version, since not . elem is
-- just as good.

"notElem -> fusible" [~1] forall x xs.
    notElem x xs = Stream.notElem x (stream xs)
"notElem -> unfused" [1] forall x xs.
    Stream.notElem x (stream xs) = notElem x xs
  -}

-- | /O(n)/,/fusion/. 'lookup' @key assocs@ looks up a key in an association list.
lookup :: Eq a => a -> [(a, b)] -> Maybe b
lookup _   []       = Nothing
lookup key xys0     = go xys0
  where
    go []           = Nothing
    go ((x,y):xys)
      | key == x    = Just y
      | otherwise   = lookup key xys
{-# NOINLINE [1] lookup #-}

{-# RULES
"lookup -> fusible" [~1] forall x xs.
    lookup x xs = Stream.lookup x (stream xs)
--"lookup -> unfused" [1] forall x xs.
--    Stream.lookup x (stream xs) = lookup x xs
  #-}

-- | /O(n)/,/fusion/. 'filter', applied to a predicate and a list, returns the list of
-- those elements that satisfy the predicate; i.e.,
--
-- > filter p xs = [ x | x <- xs, p x]
--
-- Properties:
--
-- > filter p (filter q s) = filter (\x -> q x && p x) s
--
filter :: (a -> Bool) -> [a] -> [a]
filter _ []       = []
filter p xs0      = go xs0
  where
    go []         = []
    go (x:xs)
      | p x       = x : go xs
      | otherwise =     go xs
{-# NOINLINE [1] filter #-}

{-# RULES
"filter -> fusible" [~1] forall f xs.
    filter f xs = unstream (Stream.filter f (stream xs))
--"filter -> unfused" [1] forall f xs.
--    unstream (Stream.filter f (stream xs)) = filter f xs
  #-}

------------------------------------------------------------------------
-- ** Searching with a predicate

-- | /O(n)/,/fusion/. The 'find' function takes a predicate and a list and returns the
-- first element in the list matching the predicate, or 'Nothing' if
-- there is no such element.
find :: (a -> Bool) -> [a] -> Maybe a
find _ []       = Nothing
find p xs0      = go xs0
  where
    go []                 = Nothing
    go (x:xs) | p x       = Just x
              | otherwise = go xs
{-# NOINLINE [1] find #-}

{-# RULES
"find -> fusible" [~1] forall f xs.
    find f xs = Stream.find f (stream xs)
--"find -> unfused" [1] forall f xs.
--    Stream.find f (stream xs) = find f xs
  #-}

-- | The 'partition' function takes a predicate a list and returns
-- the pair of lists of elements which do and do not satisfy the
-- predicate, respectively; i.e.,
--
-- > partition p xs == (filter p xs, filter (not . p) xs)
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p xs = foldr (select p) ([],[]) xs
{-# INLINE partition #-}

-- TODO fuse

select :: (a -> Bool) -> a -> ([a], [a]) -> ([a], [a])
select p x ~(ts,fs) | p x       = (x:ts,fs)
                    | otherwise = (ts, x:fs)

------------------------------------------------------------------------
-- * Indexing lists

-- | /O(n)/,/fusion/. List index (subscript) operator, starting from 0.
-- It is an instance of the more general 'Data.List.genericIndex',
-- which takes an index of any integral type.
(!!) :: [a] -> Int -> a
xs0 !! n0
  | n0 < 0    = error "Prelude.(!!): negative index"
  | otherwise = index xs0 n0
#ifndef __HADDOCK__
  where
    index []     _ = error "Prelude.(!!): index too large"
    index (y:ys) n = if n == 0 then y else index ys (n-1)
#endif
{-# NOINLINE [1] (!!) #-}

{-# RULES
"!! -> fusible" [~1] forall xs n.
    xs !! n = Stream.index (stream xs) n
-- "!! -> unfused" [1] forall  xs n.
--     Stream.index (stream xs) n = xs !! n
  #-}

-- | The 'elemIndex' function returns the index of the first element
-- in the given list which is equal (by '==') to the query element,
-- or 'Nothing' if there is no such element.
-- 
-- Properties:
--
-- > elemIndex x xs = listToMaybe [ n | (n,a) <- zip [0..] xs, a == x ]
-- > elemIndex x xs = findIndex (x==) xs
--
elemIndex	:: Eq a => a -> [a] -> Maybe Int
elemIndex x     = findIndex (x==)
{-# INLINE elemIndex #-}
{-
elemIndex :: Eq a => a -> [a] -> Maybe Int
elemIndex y xs0 = loop_elemIndex xs0 0
#ifndef __HADDOCK__
  where
    loop_elemIndex []     !_ = Nothing
    loop_elemIndex (x:xs) !n
      | p x       = Just n
      | otherwise = loop_elemIndex xs (n + 1)
    p = (y ==)
#endif
{-# NOINLINE [1] elemIndex #-}
-}
{- RULES
"elemIndex -> fusible" [~1] forall x xs.
    elemIndex x xs = Stream.elemIndex x (stream xs)
"elemIndex -> unfused" [1] forall x xs.
    Stream.elemIndex x (stream xs) = elemIndex x xs
  -}

-- | /O(n)/,/fusion/. The 'elemIndices' function extends 'elemIndex', by
-- returning the indices of all elements equal to the query element, in
-- ascending order.
--
-- Properties:
--
-- > length (filter (==a) xs) = length (elemIndices a xs)
--
elemIndices     :: Eq a => a -> [a] -> [Int]
elemIndices x   = findIndices (x==)
{-# INLINE elemIndices #-}

{-
elemIndices :: Eq a => a -> [a] -> [Int]
elemIndices y xs0 = loop_elemIndices xs0 0
#ifndef __HADDOCK__
  where
    loop_elemIndices []     !_  = []
    loop_elemIndices (x:xs) !n
      | p x       = n : loop_elemIndices xs (n + 1)
      | otherwise =     loop_elemIndices xs (n + 1)
    p = (y ==)
#endif
{-# NOINLINE [1] elemIndices #-}
-}
{- RULES
"elemIndices -> fusible" [~1] forall x xs.
    elemIndices x xs = unstream (Stream.elemIndices x (stream xs))
"elemIndices -> unfused" [1] forall x xs.
    unstream (Stream.elemIndices x (stream xs)) = elemIndices x xs
  -}

-- | The 'findIndex' function takes a predicate and a list and returns
-- the index of the first element in the list satisfying the predicate,
-- or 'Nothing' if there is no such element.
--
-- Properties:
--
-- > findIndex p xs = listToMaybe [ n | (n,x) <- zip [0..] xs, p x ]
--
findIndex :: (a -> Bool) -> [a] -> Maybe Int
findIndex p ls    = loop_findIndex ls 0#
  where
    loop_findIndex []   _ = Nothing
    loop_findIndex (x:xs) n
      | p x       = Just (I# n)
      | otherwise = loop_findIndex xs (n +# 1#)
{-# NOINLINE [1] findIndex #-}

{-# RULES
"findIndex -> fusible" [~1] forall f xs.
    findIndex f xs = Stream.findIndex f (stream xs)
-- "findIndex -> unfused" [1] forall f xs.
--     Stream.findIndex f (stream xs) = findIndex f xs
  #-}

-- | /O(n)/,/fusion/. The 'findIndices' function extends 'findIndex', by
-- returning the indices of all elements satisfying the predicate, in
-- ascending order.
--
-- Properties:
--
-- > length (filter p xs) = length (findIndices p xs)
--
findIndices :: (a -> Bool) -> [a] -> [Int]
findIndices p ls  = loop_findIndices ls 0#
  where
    loop_findIndices []     _ = []
    loop_findIndices (x:xs) n
      | p x       = I# n : loop_findIndices xs (n +# 1#)
      | otherwise =        loop_findIndices xs (n +# 1#)
{-# NOINLINE [1] findIndices #-}

{-# RULES
"findIndices -> fusible" [~1] forall p xs.
    findIndices p xs = unstream (Stream.findIndices p (stream xs))
-- "findIndices -> unfused" [1]  forall p xs.
--     unstream (Stream.findIndices p (stream xs)) = findIndices p xs
  #-}

------------------------------------------------------------------------
-- * Zipping and unzipping lists

-- | /O(n)/,/fusion/. 'zip' takes two lists and returns a list of
-- corresponding pairs. If one input list is short, excess elements of
-- the longer list are discarded.
--
-- Properties:
--
-- > zip a b = zipWith (,) a b
--
zip :: [a] -> [b] -> [(a, b)]
zip (a:as) (b:bs) = (a,b) : zip as bs
zip _      _      = []
{-# NOINLINE [1] zip #-}

{-# RULES
"zip -> fusible" [~1] forall xs ys.
    zip xs ys = unstream (Stream.zip (stream xs) (stream ys))
-- "zip -> unfused" [1]  forall xs ys.
--     unstream (Stream.zip (stream xs) (stream ys)) = zip xs ys
  #-}

-- | /O(n)/,/fusion/. 'zip3' takes three lists and returns a list of
-- triples, analogous to 'zip'.
--
-- Properties:
--
-- > zip3 a b c = zipWith (,,) a b c
--
zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
zip3 (a:as) (b:bs) (c:cs) = (a,b,c) : zip3 as bs cs
zip3 _      _      _      = []
{-# NOINLINE [1] zip3 #-}

{-# RULES
"zip3 -> fusible" [~1] forall xs ys zs.
    zip3 xs ys zs = unstream (Stream.zipWith3 (,,) (stream xs) (stream ys) (stream zs))
-- "zip3 -> unfused" [1]  forall xs ys zs.
--     unstream (Stream.zipWith3 (,,) (stream xs) (stream ys) (stream zs)) = zip3 xs ys zs
  #-}

-- | /O(n)/,/fusion/. The 'zip4' function takes four lists and returns a list of
-- quadruples, analogous to 'zip'.
zip4 :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip4 = zipWith4 (,,,)
{-# INLINE zip4 #-}

-- | The 'zip5' function takes five lists and returns a list of
-- five-tuples, analogous to 'zip'.
zip5 :: [a] -> [b] -> [c] -> [d] -> [e] -> [(a, b, c, d, e)]
zip5 = zipWith5 (,,,,)

-- | The 'zip6' function takes six lists and returns a list of six-tuples,
-- analogous to 'zip'.
zip6 :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [(a, b, c, d, e, f)]
zip6 = zipWith6 (,,,,,)

-- | The 'zip7' function takes seven lists and returns a list of
-- seven-tuples, analogous to 'zip'.
zip7 :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [(a, b, c, d, e, f, g)]
zip7 = zipWith7 (,,,,,,)

-- | /O(n)/,/fusion/. 'zipWith' generalises 'zip' by zipping with the
-- function given as the first argument, instead of a tupling function.
-- For example, @'zipWith' (+)@ is applied to two lists to produce the
-- list of corresponding sums.
-- Properties:
--
-- > zipWith (,) = zip
--
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f (a:as) (b:bs) = f a b : zipWith f as bs
zipWith _ _      _      = []
{-# INLINE [1] zipWith #-}

--FIXME: If we change the above INLINE to NOINLINE then ghc goes into
--       a loop, why? Do we have some dodgy recursive rules somewhere?

{-# RULES
"zipWith -> fusible" [~1] forall f xs ys.
    zipWith f xs ys = unstream (Stream.zipWith f (stream xs) (stream ys))
-- "zipWith -> unfused" [1]  forall f xs ys.
--     unstream (Stream.zipWith f (stream xs) (stream ys)) = zipWith f xs ys
  #-}

-- | /O(n)/,/fusion/. The 'zipWith3' function takes a function which
-- combines three elements, as well as three lists and returns a list of
-- their point-wise combination, analogous to 'zipWith'.
--
-- Properties:
--
-- > zipWith3 (,,) = zip3
--
zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3 z (a:as) (b:bs) (c:cs) = z a b c : zipWith3 z as bs cs
zipWith3 _ _ _ _                = []
{-# NOINLINE [1] zipWith3 #-}

{-# RULES
"zipWith3 -> fusible" [~1] forall f xs ys zs.
    zipWith3 f xs ys zs = unstream (Stream.zipWith3 f (stream xs) (stream ys) (stream zs))
-- "zipWith3 -> unfused" [1]  forall f xs ys zs.
--     unstream (Stream.zipWith3 f (stream xs) (stream ys) (stream zs)) = zipWith3 f xs ys zs
  #-}

-- | /O(n)/,/fusion/. The 'zipWith4' function takes a function which combines four
-- elements, as well as four lists and returns a list of their point-wise
-- combination, analogous to 'zipWith'.
zipWith4 :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
zipWith4 z (a:as) (b:bs) (c:cs) (d:ds)
                        = z a b c d : zipWith4 z as bs cs ds
zipWith4 _ _ _ _ _      = []
{-# NOINLINE [1] zipWith4 #-}

{-# RULES
"zipWith4 -> fusible" [~1] forall f ws xs ys zs.
    zipWith4 f ws xs ys zs = unstream (Stream.zipWith4 f (stream ws) (stream xs) (stream ys) (stream zs))
-- "zipWith4 -> unfused" [1]  forall f ws xs ys zs.
--     unstream (Stream.zipWith4 f (stream ws) (stream xs) (stream ys) (stream zs)) = zipWith4 f ws xs ys zs
  #-}

-- | The 'zipWith5' function takes a function which combines five
-- elements, as well as five lists and returns a list of their point-wise
-- combination, analogous to 'zipWith'.
zipWith5 :: (a -> b -> c -> d -> e -> f)
         -> [a] -> [b] -> [c] -> [d] -> [e] -> [f]
zipWith5 z (a:as) (b:bs) (c:cs) (d:ds) (e:es)
                        = z a b c d e : zipWith5 z as bs cs ds es
zipWith5 _ _ _ _ _ _    = []

-- TODO fuse

-- | The 'zipWith6' function takes a function which combines six
-- elements, as well as six lists and returns a list of their point-wise
-- combination, analogous to 'zipWith'.
zipWith6 :: (a -> b -> c -> d -> e -> f -> g)
         -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g]
zipWith6 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs)
                        = z a b c d e f : zipWith6 z as bs cs ds es fs
zipWith6 _ _ _ _ _ _ _  = []

-- TODO fuse

-- | The 'zipWith7' function takes a function which combines seven
-- elements, as well as seven lists and returns a list of their point-wise
-- combination, analogous to 'zipWith'.
zipWith7 :: (a -> b -> c -> d -> e -> f -> g -> h)
         -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [h]
zipWith7 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs)
                         = z a b c d e f g : zipWith7 z as bs cs ds es fs gs
zipWith7 _ _ _ _ _ _ _ _ = []

-- TODO fuse

------------------------------------------------------------------------
-- unzips

-- | 'unzip' transforms a list of pairs into a list of first components
-- and a list of second components.
unzip :: [(a, b)] -> ([a], [b])
unzip = foldr (\(a,b) ~(as,bs) -> (a:as,b:bs)) ([],[])

-- TODO fuse

-- | The 'unzip3' function takes a list of triples and returns three
-- lists, analogous to 'unzip'.
unzip3 :: [(a, b, c)] -> ([a], [b], [c])
unzip3 = foldr (\(a,b,c) ~(as,bs,cs) -> (a:as,b:bs,c:cs)) ([],[],[])

-- TODO fuse

-- | The 'unzip4' function takes a list of quadruples and returns four
-- lists, analogous to 'unzip'.
unzip4 :: [(a, b, c, d)] -> ([a], [b], [c], [d])
unzip4 = foldr (\(a,b,c,d) ~(as,bs,cs,ds) ->
                      (a:as,b:bs,c:cs,d:ds))
               ([],[],[],[])

-- TODO fuse

-- | The 'unzip5' function takes a list of five-tuples and returns five
-- lists, analogous to 'unzip'.
unzip5 :: [(a, b, c, d, e)] -> ([a], [b], [c], [d], [e])
unzip5 = foldr (\(a,b,c,d,e) ~(as,bs,cs,ds,es) ->
                      (a:as,b:bs,c:cs,d:ds,e:es))
               ([],[],[],[],[])

-- TODO fuse

-- | The 'unzip6' function takes a list of six-tuples and returns six
-- lists, analogous to 'unzip'.
unzip6 :: [(a, b, c, d, e, f)] -> ([a], [b], [c], [d], [e], [f])
unzip6 = foldr (\(a,b,c,d,e,f) ~(as,bs,cs,ds,es,fs) ->
                      (a:as,b:bs,c:cs,d:ds,e:es,f:fs))
               ([],[],[],[],[],[])

-- TODO fuse

-- | The 'unzip7' function takes a list of seven-tuples and returns
-- seven lists, analogous to 'unzip'.
unzip7 :: [(a, b, c, d, e, f, g)] -> ([a], [b], [c], [d], [e], [f], [g])
unzip7 = foldr (\(a,b,c,d,e,f,g) ~(as,bs,cs,ds,es,fs,gs) ->
                      (a:as,b:bs,c:cs,d:ds,e:es,f:fs,g:gs))
               ([],[],[],[],[],[],[])

-- TODO fuse

------------------------------------------------------------------------
-- * Special lists
-- ** Functions on strings

-- | /O(O)/,/fusion/. 'lines' breaks a string up into a list of strings
-- at newline characters. The resulting strings do not contain
-- newlines.
lines :: String -> [String]
lines [] = []
lines s  = let (l, s') = break (== '\n') s
            in l : case s' of
                     []      -> []
                     (_:s'') -> lines s''
--TODO: can we do better than this and preserve the same strictness?

{-
-- This implementation is fast but too strict :-(
-- it doesn't yield each line until it has seen the ending '\n'

lines :: String -> [String]
lines []  = []
lines cs0 = go [] cs0
  where
    go l []        = reverse l : []
    go l ('\n':cs) = reverse l : case cs of
                                   [] -> []
                                   _  -> go [] cs
    go l (  c :cs) = go (c:l) cs
-}
{-# INLINE [1] lines #-}

{- RULES
"lines -> fusible" [~1] forall xs.
    lines xs = unstream (Stream.lines (stream xs))
"lines -> unfused" [1]  forall xs.
    unstream (Stream.lines (stream xs)) = lines xs
  -}

-- | 'words' breaks a string up into a list of words, which were delimited
-- by white space.
words :: String -> [String]
words s = case dropWhile isSpace s of
            "" -> []
            s' -> w : words s''
                  where (w, s'') = break isSpace s'
-- TODO fuse
--TODO: can we do better than this and preserve the same strictness?

{-
-- This implementation is fast but too strict :-(
-- it doesn't yield each word until it has seen the ending space

words cs0 = dropSpaces cs0
  where
    dropSpaces :: String -> [String]
    dropSpaces []         = []
    dropSpaces (c:cs)
         | isSpace c = dropSpaces cs
         | otherwise      = munchWord [c] cs

    munchWord :: String -> String -> [String]
    munchWord w []     = reverse w : []
    munchWord w (c:cs)
      | isSpace c = reverse w : dropSpaces cs
      | otherwise      = munchWord (c:w) cs
-}

-- | /O(n)/,/fusion/. 'unlines' is an inverse operation to 'lines'.
-- It joins lines, after appending a terminating newline to each.
--
-- > unlines xs = concatMap (++"\n")
--
unlines :: [String] -> String
unlines css0 = to css0
  where go []     css = '\n' : to css
        go (c:cs) css =   c  : go cs css

        to []       = []
        to (cs:css) = go cs css
{-# NOINLINE [1] unlines #-}

--
-- fuse via:
--      unlines xs = concatMap (snoc xs '\n')
--
{- RULES
"unlines -> fusible" [~1] forall xs.
    unlines xs = unstream (Stream.concatMap (\x -> Stream.snoc (stream x) '\n') (stream xs))
"unlines -> unfused" [1]  forall xs.
    unstream (Stream.concatMap (\x -> Stream.snoc (stream x) '\n') (stream xs)) = unlines xs
  -}

-- | 'unwords' is an inverse operation to 'words'.
-- It joins words with separating spaces.
unwords :: [String] -> String
unwords []         = []
unwords (cs0:css0) = go cs0 css0
  where go []     css = to css
        go (c:cs) css = c : go cs css

        to []       = []
        to (cs:ccs) = ' ' : go cs ccs

-- TODO fuse

------------------------------------------------------------------------
-- ** \"Set\" operations

-- | The 'nub' function removes duplicate elements from a list.
-- In particular, it keeps only the first occurrence of each element.
-- (The name 'nub' means \`essence\'.)
-- It is a special case of 'nubBy', which allows the programmer to supply
-- their own equality test.
--
nub :: Eq a => [a] -> [a]
nub l               = nub' l []
  where
    nub' [] _       = []
    nub' (x:xs) ls
      | x `elem` ls = nub' xs ls
      | otherwise   = x : nub' xs (x:ls)

{- RULES
-- ndm's optimisation
"sort/nub" forall xs.  sort (nub xs) = map head (group (sort xs))
  -}

-- TODO fuse

-- | 'delete' @x@ removes the first occurrence of @x@ from its list argument.
-- For example,
--
-- > delete 'a' "banana" == "bnana"
--
-- It is a special case of 'deleteBy', which allows the programmer to
-- supply their own equality test.
--
delete :: Eq a => a -> [a] -> [a]
delete = deleteBy (==)

-- TODO fuse

-- | The '\\' function is list difference ((non-associative).
-- In the result of @xs@ '\\' @ys@, the first occurrence of each element of
-- @ys@ in turn (if any) has been removed from @xs@.  Thus
--
-- > (xs ++ ys) \\ xs == ys.
--
-- It is a special case of 'deleteFirstsBy', which allows the programmer
-- to supply their own equality test.
(\\) :: Eq a => [a] -> [a] -> [a]
(\\) = foldl (flip delete)

-- | The 'union' function returns the list union of the two lists.
-- For example,
--
-- > "dog" `union` "cow" == "dogcw"
--
-- Duplicates, and elements of the first list, are removed from the
-- the second list, but if the first list contains duplicates, so will
-- the result.
-- It is a special case of 'unionBy', which allows the programmer to supply
-- their own equality test.
--
union :: Eq a => [a] -> [a] -> [a]
union = unionBy (==)

-- TODO fuse

-- | The 'intersect' function takes the list intersection of two lists.
-- For example,
--
-- > [1,2,3,4] `intersect` [2,4,6,8] == [2,4]
--
-- If the first list contains duplicates, so will the result.
-- It is a special case of 'intersectBy', which allows the programmer to
-- supply their own equality test.
--
intersect :: Eq a => [a] -> [a] -> [a]
intersect = intersectBy (==)

-- TODO fuse

------------------------------------------------------------------------
-- ** Ordered lists 

-- TODO stuff in Ord can use Map/IntMap
-- TODO Hooray, an Ord constraint! we could use a better structure.

-- | The 'sort' function implements a stable sorting algorithm.
-- It is a special case of 'sortBy', which allows the programmer to supply
-- their own comparison function.
--
-- Properties:
--
-- > not (null x) ==> (head . sort) x = minimum x
-- > not (null x) ==> (last . sort) x = maximum x
--
sort :: Ord a => [a] -> [a]
sort l = mergesort compare l

-- TODO fuse, we have an Ord constraint!

-- | /O(n)/,/fusion/. The 'insert' function takes an element and a list and inserts the
-- element into the list at the last position where it is still less
-- than or equal to the next element.  In particular, if the list
-- is sorted before the call, the result will also be sorted.
-- It is a special case of 'insertBy', which allows the programmer to
-- supply their own comparison function.
--
insert :: Ord a => a -> [a] -> [a]
insert e ls = insertBy (compare) e ls
{-# INLINE insert #-}

------------------------------------------------------------------------
-- * Generalized functions
-- ** The \"By\" operations
-- *** User-supplied equality (replacing an Eq context)

-- | The 'nubBy' function behaves just like 'nub', except it uses a
-- user-supplied equality predicate instead of the overloaded '=='
-- function.
nubBy :: (a -> a -> Bool) -> [a] -> [a]
nubBy eq l              = nubBy' l []
  where
    nubBy' [] _         = []
    nubBy' (y:ys) xs
      | elem_by eq y xs = nubBy' ys xs
      | otherwise       = y : nubBy' ys (y:xs)

-- TODO fuse

-- Not exported:
-- Note that we keep the call to `eq` with arguments in the
-- same order as in the reference implementation
-- 'xs' is the list of things we've seen so far, 
-- 'y' is the potential new element
--
elem_by :: (a -> a -> Bool) -> a -> [a] -> Bool
elem_by _  _ []         = False
elem_by eq y (x:xs)     = if x `eq` y then True else elem_by eq y xs

-- | The 'deleteBy' function behaves like 'delete', but takes a
-- user-supplied equality predicate.
deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy _  _ []        = []
deleteBy eq x (y:ys)    = if x `eq` y then ys else y : deleteBy eq x ys

-- TODO fuse

deleteFirstsBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
deleteFirstsBy eq       = foldl (flip (deleteBy eq))


-- | The 'unionBy' function is the non-overloaded version of 'union'.
unionBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
unionBy eq xs ys        = xs ++ foldl (flip (deleteBy eq)) (nubBy eq ys) xs

-- TODO fuse

-- | The 'intersectBy' function is the non-overloaded version of 'intersect'.
intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
intersectBy eq xs ys    = [x | x <- xs, any (eq x) ys]

-- TODO fuse

-- | The 'groupBy' function is the non-overloaded version of 'group'.
groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _  []     = []
groupBy eq (x:xs) = (x:ys) : groupBy eq zs
                    where (ys,zs) = span (eq x) xs

-- TODO fuse

------------------------------------------------------------------------
-- *** User-supplied comparison (replacing an Ord context)

-- | The 'sortBy' function is the non-overloaded version of 'sort'.
sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy cmp l = mergesort cmp l

-- TODO fuse

mergesort :: (a -> a -> Ordering) -> [a] -> [a]
mergesort cmp xs = mergesort' cmp (map wrap xs)

mergesort' :: (a -> a -> Ordering) -> [[a]] -> [a]
mergesort' _ []    = []
mergesort' _ [xs]  = xs
mergesort' cmp xss = mergesort' cmp (merge_pairs cmp xss)

merge_pairs :: (a -> a -> Ordering) -> [[a]] -> [[a]]
merge_pairs _   []          = []
merge_pairs _   [xs]        = [xs]
merge_pairs cmp (xs:ys:xss) = merge cmp xs ys : merge_pairs cmp xss

merge :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
merge _   xs [] = xs
merge _   [] ys = ys
merge cmp (x:xs) (y:ys)
 = case x `cmp` y of
        GT -> y : merge cmp (x:xs)   ys
        _  -> x : merge cmp    xs (y:ys)

wrap :: a -> [a]
wrap x = [x]

-- | /O(n)/,/fusion/. The non-overloaded version of 'insert'.
insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy _   x [] = [x]
insertBy cmp x ys@(y:ys')
    = case cmp x y of
        GT -> y : insertBy cmp x ys'
        _  -> x : ys
{-# NOINLINE [1] insertBy #-}

{-# RULES
"insertBy -> fusible" [~1] forall f x xs.
    insertBy f x xs = unstream (Stream.insertBy f x (stream xs))
-- "insertBy -> unfused" [1]  forall f x xs.
--     unstream (Stream.insertBy f x (stream xs)) = insertBy f x xs
  #-}

-- | /O(n)/,/fusion/. The 'maximumBy' function takes a comparison function and a list
-- and returns the greatest element of the list by the comparison function.
-- The list must be finite and non-empty.
--
maximumBy :: (a -> a -> Ordering) -> [a] -> a
maximumBy _ []   = error "List.maximumBy: empty list"
maximumBy cmp xs = foldl1 max' xs
    where
       max' x y = case cmp x y of
                    GT -> x
                    _  -> y
{-# NOINLINE [1] maximumBy #-}

{-# RULES
"maximumBy -> fused"  [~1] forall p xs.
    maximumBy p xs = Stream.maximumBy p (stream xs)
-- "maximumBy -> unfused" [1] forall p xs.
--     Stream.maximumBy p (stream xs) = maximumBy p xs
  #-}

-- | /O(n)/,/fusion/. The 'minimumBy' function takes a comparison function and a list
-- and returns the least element of the list by the comparison function.
-- The list must be finite and non-empty.
minimumBy :: (a -> a -> Ordering) -> [a] -> a
minimumBy _ []   = error "List.minimumBy: empty list"
minimumBy cmp xs = foldl1 min' xs
    where
        min' x y = case cmp x y of
                        GT -> y
                        _  -> x
{-# NOINLINE [1] minimumBy #-}

{-# RULES
"minimumBy -> fused"  [~1] forall p xs.
    minimumBy p xs = Stream.minimumBy p (stream xs)
-- "minimumBy -> unfused" [1] forall p xs.
--     Stream.minimumBy p (stream xs) = minimumBy p xs
  #-}

------------------------------------------------------------------------
-- * The \"generic\" operations

-- | The 'genericLength' function is an overloaded version of 'length'.  In
-- particular, instead of returning an 'Int', it returns any type which is
-- an instance of 'Num'.  It is, however, less efficient than 'length'.
--
genericLength :: Num i => [b] -> i
genericLength []    = 0
genericLength (_:l) = 1 + genericLength l
{-# NOINLINE [1] genericLength #-}

{-# RULES
"genericLength -> fusible" [~1] forall xs.
    genericLength xs = Stream.genericLength (stream xs)
-- "genericLength -> unfused" [1] forall xs.
--     Stream.genericLength (stream xs) = genericLength xs
  #-}

{-# RULES
"genericLength -> length/Int" genericLength = length :: [a] -> Int
  #-}

-- | /O(n)/,/fusion/. The 'genericTake' function is an overloaded version of 'take', which
-- accepts any 'Integral' value as the number of elements to take.
genericTake :: Integral i => i -> [a] -> [a]
genericTake 0 _      = []
genericTake _ []     = []
genericTake n (x:xs)
             | n > 0 = x : genericTake (n-1) xs
             | otherwise = error "List.genericTake: negative argument"
{-# NOINLINE [1] genericTake #-}

{-# RULES
"genericTake -> fusible" [~1] forall xs n.
    genericTake n xs = unstream (Stream.genericTake n (stream xs))
-- "genericTake -> unfused" [1]  forall xs n.
--     unstream (Stream.genericTake n (stream xs)) = genericTake n xs
  #-}

{-# RULES
"genericTake -> take/Int" genericTake = take :: Int -> [a] -> [a]
  #-}

-- | /O(n)/,/fusion/. The 'genericDrop' function is an overloaded version of 'drop', which
-- accepts any 'Integral' value as the number of elements to drop.
genericDrop :: Integral i => i -> [a] -> [a]
genericDrop 0 xs        = xs
genericDrop _ []        = []
genericDrop n (_:xs) | n > 0  = genericDrop (n-1) xs
genericDrop _ _         = error "List.genericDrop: negative argument"
{-# NOINLINE [1] genericDrop #-}

{-# RULES
"genericDrop -> fusible" [~1] forall xs n.
    genericDrop n xs = unstream (Stream.genericDrop n (stream xs))
-- "genericDrop -> unfused" [1]  forall xs n.
--     unstream (Stream.genericDrop n (stream xs)) = genericDrop n xs
  #-}

{-# RULES
"genericDrop -> drop/Int" genericDrop = drop :: Int -> [a] -> [a]
  #-}

-- | /O(n)/,/fusion/. The 'genericIndex' function is an overloaded version of '!!', which
-- accepts any 'Integral' value as the index.
genericIndex :: Integral a => [b] -> a -> b
genericIndex (x:_)  0 = x
genericIndex (_:xs) n
    | n > 0           = genericIndex xs (n-1)
    | otherwise       = error "List.genericIndex: negative argument."
genericIndex _ _      = error "List.genericIndex: index too large."
{-# NOINLINE [1] genericIndex #-}


-- can we pull the n > 0 test out and do it just once?
-- probably not since we don't know what n-1 does!!
-- can only specialise it for sane Integral instances :-(

{-# RULES
"genericIndex -> fusible" [~1] forall xs n.
    genericIndex xs n = Stream.genericIndex (stream xs) n
-- "genericIndex -> unfused" [1]  forall xs n.
--     Stream.genericIndex (stream xs) n = genericIndex n xs
  #-}

{-# RULES
"genericIndex -> index/Int" genericIndex = (!!) :: [a] -> Int -> a
  #-}

-- | /O(n)/,/fusion/. The 'genericSplitAt' function is an overloaded
-- version of 'splitAt', which accepts any 'Integral' value as the
-- position at which to split.
--
genericSplitAt :: Integral i => i -> [a] -> ([a], [a])
genericSplitAt 0 xs     = ([],xs)
genericSplitAt _ []     = ([],[])
genericSplitAt n (x:xs) | n > 0  = (x:xs',xs'')
    where (xs',xs'') = genericSplitAt (n-1) xs
genericSplitAt _ _      = error "List.genericSplitAt: negative argument"

{-# RULES
"genericSplitAt -> fusible" [~1] forall xs n.
    genericSplitAt n xs = Stream.genericSplitAt n (stream xs)
-- "genericSplitAt -> unfused" [1]  forall xs n.
--     Stream.genericSplitAt n (stream xs) = genericSplitAt n xs
  #-}

{-# RULES
"genericSplitAt -> splitAt/Int" genericSplitAt = splitAt :: Int -> [a] -> ([a], [a])
  #-}

-- | /O(n)/,/fusion/. The 'genericReplicate' function is an overloaded version of 'replicate',
-- which accepts any 'Integral' value as the number of repetitions to make.
--
genericReplicate :: Integral i => i -> a -> [a]
genericReplicate n x = genericTake n (repeat x)
{-# INLINE genericReplicate #-}

{-# RULES
"genericReplicate -> replicate/Int" genericReplicate = replicate :: Int -> a -> [a]
  #-}

-- ---------------------------------------------------------------------
-- Internal utilities

-- Common up near identical calls to `error' to reduce the number
-- constant strings created when compiled:
errorEmptyList :: String -> a
errorEmptyList fun = moduleError fun "empty list"
{-# NOINLINE errorEmptyList #-}

moduleError :: String -> String -> a
moduleError fun msg = error ("Data.List." ++ fun ++ ':':' ':msg)
{-# NOINLINE moduleError #-}

bottom :: a
bottom = error "Data.List.Stream: bottom"
{-# NOINLINE bottom #-}
