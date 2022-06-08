{-# LANGUAGE ScopedTypeVariables #-}

--
-- Test the new list implementation for internal properties
--
import Prelude (($),(.),(&&),(||),(+),Int,(==),Bool(..),not,flip,uncurry)

import Properties.Utils
import System.IO

import Data.Maybe
import qualified Data.List.Stream as T

------------------------------------------------------------------------
-- * Basic interface

-- map fusion
prop_mapmap = (\(f  :: B -> C)
                (g  :: A -> B)
                (xs) -> T.map f . T.map g $ xs) `eq3`

                 (\f g xs -> T.map (f.g)       $ xs)

prop_maprepeat n = (\(f :: A -> B)
                   (x :: A) -> T.map f (T.take n $ T.repeat x)) `eq2` (\f x -> T.take n $ T.repeat (f x))

prop_mapreplicate n = (\(f :: A -> B)
                   (x :: A) -> T.map f (T.replicate n x)) `eq2` (\f x -> T.replicate n (f x))

------------------------------------------------------------------------
-- * List transformations

prop_reverse = T.foldl (flip (:)) [] `eq1` (\(x :: [A]) -> T.reverse  x)

------------------------------------------------------------------------
-- * Reducing lists (folds)

------------------------------------------------------------------------
-- ** Special folds

prop_concatfoldr = (\(xs :: [[A]]) -> T.concat xs) `eq1` (T.foldr (T.++) [])

prop_concatmap = (\(f :: A -> [B]) (xs :: [A]) -> T.concatMap f xs) `eq2` 
                                            (\f -> T.concat . T.map f)

prop_and       = (T.foldr (&&) True) `eq1` T.and
prop_or        = (T.foldr (||) False) `eq1` T.or
prop_sum       = (T.foldl (+) 0)     `eq1`  (T.sum :: [Int] -> Int)

------------------------------------------------------------------------
-- * Building lists
-- ** Scans

-- (a -> b -> a) -> a -> [b] -> [a]
prop_scanlfold = (\(f :: A -> B -> A)
                   (z :: A)
                   (xs::[B]) ->
                        T.last (T.scanl f z xs)) `eq3`
                 (\f z xs ->
                        T.foldl f z xs)

-- (a -> b -> a) -> a -> [b] -> [a]
prop_scanrfold = (\(f :: A -> B -> B)
                   (z :: B)
                   (xs::[A]) ->
                        T.head (T.scanr f z xs)) `eq3`
                 (\f z xs ->
                        T.foldr f z xs)

------------------------------------------------------------------------
-- ** Accumulating maps

------------------------------------------------------------------------
-- ** Infinite lists

prop_replicate = T.replicate `eq2` (\n (x :: A) -> T.take n (T.repeat x))

------------------------------------------------------------------------
-- ** Unfolding

------------------------------------------------------------------------
-- * Sublists
-- ** Extracting sublists

prop_break_span = T.break `eq2` (\(p :: A -> Bool) -> T.span (not . p))

------------------------------------------------------------------------
-- * Predicates

{-
--
-- not true, has to take the minimum of the two, not the first only.
--
prop_isPrefixOfzip = (\(xs:: [A]) ys ->
                               (T.isPrefixOf xs ys)) `eq2`
                     (\xs ys ->
                               (T.all (uncurry (==)) (T.zip xs ys)))
-}

------------------------------------------------------------------------
-- * Searching lists
-- ** Searching by equality

------------------------------------------------------------------------
-- ** Searching with a predicate

prop_filterfilter = (\(p :: A -> Bool)
                      (q :: A -> Bool)
                      (s :: [A] ) ->
                             T.filter p (T.filter q s))         `eq3`
                    (\p q s ->
                             T.filter (\x -> q x && p x) s)

------------------------------------------------------------------------
-- * Indexing lists

prop_findIndexfilter = (\p (xs ::[A]) ->
                             T.findIndex p xs) `eq2`
                       (\p xs         ->
                             listToMaybe [ n | (n,x) <- T.zip [0..] xs, p x ])

prop_elemIndexfilter = (\x (xs ::[A]) ->
                             T.elemIndex x xs) `eq2`
                       (\x xs         ->
                             listToMaybe [ n | (n,a) <- T.zip [0..] xs, a==x ])

prop_elemfindIndex   = (\x (xs ::[A]) ->
                             T.findIndex (==x) xs) `eq2`
                       (\x xs         ->
                             T.elemIndex x     xs)

prop_filterelemIndices = (\a (xs :: [A]) ->
                             T.length (T.filter (==a) xs)) `eq2`
                         (\a xs ->
                             T.length (T.elemIndices a xs))

prop_filterfindIndices = (\p (xs :: [A]) ->
                             T.length (T.filter p xs)) `eq2`
                         (\p xs ->
                             T.length (T.findIndices p xs))

------------------------------------------------------------------------
-- * Zipping and unzipping lists

------------------------------------------------------------------------

------------------------------------------------------------------------
-- * Special lists
-- ** Functions on strings

prop_unlinesconcat    = T.unlines `eqnotnull1` (\xs -> T.concat (T.intersperse "\n" xs) T.++ "\n")
prop_unlinesconcatMap = T.unlines `eq1`        (\xs -> T.concatMap (T.++ "\n") xs)

------------------------------------------------------------------------
-- ** \"Set\" operations

prop_nubsort    = (\(xs :: [OrdA]) -> T.sort . T.nub $ xs )  `eq1`
                  (\xs       -> T.map T.head . T.group . T.sort $ xs)

------------------------------------------------------------------------
-- ** Ordered lists 

prop_headsort = (T.head . T.sort) `eqnotnull1` (\x -> T.minimum (x :: [OrdA]))
prop_lastsort = (T.last . T.sort) `eqnotnull1` (\x -> T.maximum (x :: [OrdA]))

------------------------------------------------------------------------
-- * Generalized functions
-- ** The \"By\" operations
-- *** User-supplied equality (replacing an Eq context)

prop_nubsortby    = (\f g (xs :: [OrdA]) -> T.sortBy f . T.nubBy g $ xs )  `eq3`
                    (\f g xs       -> T.map T.head . T.groupBy g . T.sortBy f $ xs)

------------------------------------------------------------------------
-- *** User-supplied comparison (replacing an Ord context)

------------------------------------------------------------------------
-- * The \"generic\" operations

------------------------------------------------------------------------

main = do
  hSetBuffering stdout NoBuffering

  runTests "Basic interface" opts
    [ run prop_mapmap
    , run prop_maprepeat
    , run prop_mapreplicate
    ]

  runTests "List transformations" opts
    [ run prop_reverse
    ]

{-
  runTests "Reducing lists (folds)" opts
    [
    ]
-}

  runTests "Special folds" opts
    [ run prop_concatfoldr
    , run prop_concatmap
    , run prop_and
    , run prop_or
    , run prop_sum
    ]

  runTests "Scans" opts
    [ run prop_scanlfold
    , run prop_scanrfold
    ]

{-
  runTests "Accumulating maps" opts
    [
    ]
-}

  runTests "Infinite lists" opts
    [ run prop_replicate
    ]

{-
  runTests "Unfolding" opts
    [
    ]
-}

  runTests "Extracting sublists" opts
    [ run prop_break_span
    ]

{-
  runTests "Predicates" opts
    [ run prop_isPrefixOfzip
    ]
-}

{-
  runTests "Searching by equality" opts
    [
    ]
-}

  runTests "Searching by a predicate" opts
    [ run prop_filterfilter
    ]

  runTests "Indexing lists" opts
    [ run prop_filterelemIndices
    , run prop_filterfindIndices
    , run prop_findIndexfilter
    , run prop_elemIndexfilter
    , run prop_elemfindIndex
    ]

{-
  runTests "Zipping" opts
    [
    ]
-}

{-
  runTests "Unzipping" opts
    [
    ]
-}

  runTests "Functions on strings" opts
    [ run prop_unlinesconcat
    , run prop_unlinesconcatMap
    ]

  runTests "\"Set\" operations" opts
    [ run prop_nubsort
    ]

  runTests "Ordered lists" opts
    [ run prop_headsort
    , run prop_lastsort
    ]


  runTests "Eq style \"By\" operations" opts
    [ run prop_nubsortby     -- same issue as prop_sortBy in ListProperties
    ]
{-
  runTests "Ord style \"By\" operations" opts
    [
    ]

  runTests "The \"generic\" operations" opts
    [
    ]
-}

------------------------------------------------------------------------

{-
  runTests "Searching by equality" opts
    [
    ]
-}

  runTests "Searching by a predicate" opts
    [ run prop_filterfilter
    ]

  runTests "Indexing lists" opts
    [ run prop_filterelemIndices
    , run prop_filterfindIndices
    , run prop_findIndexfilter
    , run prop_elemIndexfilter
    , run prop_elemfindIndex
    ]

{-
  runTests "Zipping" opts
    [
    ]
-}

{-
  runTests "Unzipping" opts
    [
    ]
-}

  runTests "Functions on strings" opts
    [ run prop_unlinesconcat
    , run prop_unlinesconcatMap
    ]

  runTests "\"Set\" operations" opts
    [ run prop_nubsort
    ]

  runTests "Ordered lists" opts
    [ run prop_headsort
    , run prop_lastsort
    ]


  runTests "Eq style \"By\" operations" opts
    [ run prop_nubsortby     -- same issue as prop_sortBy in ListProperties
    ]
{-
  runTests "Ord style \"By\" operations" opts
    [
    ]

  runTests "The \"generic\" operations" opts
    [
    ]
-}

------------------------------------------------------------------------
