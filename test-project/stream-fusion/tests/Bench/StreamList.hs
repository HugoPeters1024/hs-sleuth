--
-- list like wrappers for abstract streams
--
-- These specify the versions used when fusion occurs.
--
-- So we can check our stream implementations, which are only used when
-- fusion happens, are fast.
--

module Bench.StreamList where

import Prelude
import qualified Prelude 
import Properties.Utils

import qualified Data.Stream as Stream

-- * Basic interface
cons            :: a -> [a] -> [a]
snoc            :: [a] -> a -> [a]
append          :: [a] -> [a] -> [a]
head            :: [a] -> a
last            :: [a] -> a
tail            :: [a] -> [a]
init            :: [a] -> [a]
null            :: [a] -> Bool
length          :: [a] -> Int


-- * List transformations
map             :: (a -> b) -> [a] -> [b]
--reverse       :: [a] -> [a]
intersperse     :: a -> [a] -> [a]
intercalate   :: [a] -> [[a]] -> [a]
--transpose     :: [[a]] -> [[a]]

-- * Reducing lists (folds)
foldl           :: (b -> a -> b) -> b -> [a] -> b
foldl'          :: (b -> a -> b) -> b -> [a] -> b
foldl1          :: (a -> a -> a) -> [a] -> a
foldl1'         :: (a -> a -> a) -> [a] -> a
foldr           :: (a -> b -> b) -> b -> [a] -> b
foldr1          :: (a -> a -> a) -> [a] -> a

-- ** Special folds
concat          :: [[a]] -> [a]
concatMap       :: (a -> [b]) -> [a] -> [b]
and             :: [Bool] -> Bool
or              :: [Bool] -> Bool
any             :: (a -> Bool) -> [a] -> Bool
all             :: (a -> Bool) -> [a] -> Bool
sum             :: [N] -> N
product         :: [N] -> N
maximum         :: Ord a => [a] -> a
minimum         :: Ord a => [a] -> a

-- * Building lists
-- ** Scans
scanl           :: (a -> b -> a) -> a -> [b] -> [a]
scanl1          :: (a -> a -> a) -> [a] -> [a]

{-
scanr           :: (a -> b -> b) -> b -> [a] -> [b]
scanr1          :: (a -> a -> a) -> [a] -> [a]

-- ** Accumulating maps
mapAccumL       :: (c -> a -> (c, b)) -> c -> [a] -> (c, [b])
mapAccumR       :: (c -> a -> (c, b)) -> c -> [a] -> (c, [b])
-}

-- ** Infinite lists
iterate         :: (a -> a) -> a -> [a]
repeat          :: a -> [a]
replicate       :: Int -> a -> [a]
cycle           :: [a] -> [a]

-- ** Unfolding
unfoldr         :: (b -> Maybe (a, b)) -> b -> [a]

-- * Sublists
-- ** Extracting sublists
take            :: Int -> [a] -> [a]
drop            :: Int -> [a] -> [a]
splitAt         :: Int -> [a] -> ([a], [a])
takeWhile       :: (a -> Bool) -> [a] -> [a]
dropWhile       :: (a -> Bool) -> [a] -> [a]

{-
span          :: (a -> Bool) -> [a] -> ([a], [a])
break           :: (a -> Bool) -> [a] -> ([a], [a])
group           :: [a] -> [[a]]
inits           :: [a] -> [[a]]
tails           :: [a] -> [[a]]
-}

-- * Predicates
isPrefixOf      :: Eq a => [a] -> [a] -> Bool
{-
isSuffixOf      :: [a] -> [a] -> Bool
isInfixOf       :: [a] -> [a] -> Bool
-}

-- * Searching lists
-- ** Searching by equality
elem            :: Eq a => a -> [a] -> Bool
--notElem       :: Eq a => a -> [a] -> Bool
lookup          :: Eq a => a -> [(a, b)] -> Maybe b

-- ** Searching with a predicate
find            :: (a -> Bool) -> [a] -> Maybe a
filter          :: (a -> Bool) -> [a] -> [a]
--partition     :: (a -> Bool) -> [a] -> ([a], [a])

-- * Indexing lists
(!!)            :: [a] -> Int -> a
findIndex       :: (a -> Bool) -> [a] -> Maybe Int
elemIndex       :: Eq a => a -> [a] -> Maybe Int
elemIndices     :: Eq a => a -> [a] -> [Int]
findIndices     :: (a -> Bool) -> [a] -> [Int]

-- * Zipping and unzipping lists
zip             :: [a] -> [b] -> [(a, b)]
zip3            :: [a] -> [b] -> [c] -> [(a, b, c)]
{-
zip4            :: [a] -> [b] -> [c] -> [d] -> [(a, b, c, d)]
zip5            :: [a] -> [b] -> [c] -> [d] -> [e] -> [(a, b, c, d, e)]
zip6            :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [(a, b, c, d, e, f)]
zip7            :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [(a, b, c, d, e, f, g)]
-}

zipWith         :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith3        :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
{-
zipWith4        :: (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
zipWith5        :: (a -> b -> c -> d -> e -> f) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f]
zipWith6        :: (a -> b -> c -> d -> e -> f -> g) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g]
zipWith7        :: (a -> b -> c -> d -> e -> f -> g -> h) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [h]
-}

unzip           :: [(a, b)] -> ([a], [b])
{-
unzip3          :: [(a, b, c)] -> ([a], [b], [c])
unzip4          :: [(a, b, c, d)] -> ([a], [b], [c], [d])
unzip5          :: [(a, b, c, d, e)] -> ([a], [b], [c], [d], [e])
unzip6          :: [(a, b, c, d, e, f)] -> ([a], [b], [c], [d], [e], [f])
unzip7          :: [(a, b, c, d, e, f, g)] -> ([a], [b], [c], [d], [e], [f], [g])
-}

-- * Special lists
-- ** Functions on strings
{-
unlines         :: [String] -> String
lines           :: String -> [String]
-}
{-
words           :: String -> [String]
unwords         :: [String] -> String

-- ** \"Set\" operations
nub             :: [a] -> [a]
delete          :: a -> [a] -> [a]
(\\)            :: [a] -> [a] -> [a]
union           :: [a] -> [a] -> [a]
intersect       :: [a] -> [a] -> [a]

-- ** Ordered lists 
sort            :: [a] -> [a]
insert          :: a -> [a] -> [a]

-- * Generalized functions
-- ** The \"By\" operations
-- *** User-supplied equality (replacing an Eq context)
nubBy           :: (a -> a -> Bool) -> [a] -> [a]
deleteBy        :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteFirstsBy  :: (a -> a -> Bool) -> [a] -> [a] -> [a]
unionBy         :: (a -> a -> Bool) -> [a] -> [a] -> [a]
intersectBy     :: (a -> a -> Bool) -> [a] -> [a] -> [a]
groupBy         :: (a -> a -> Bool) -> [a] -> [[a]]
-}

-- *** User-supplied comparison (replacing an Ord context)
insertBy        :: (a -> a -> Ordering) -> a -> [a] -> [a]
{-
sortBy          :: (a -> a -> Ordering) -> [a] -> [a]
-}
maximumBy       :: (a -> a -> Ordering) -> [a] -> a
minimumBy       :: (a -> a -> Ordering) -> [a] -> a

-- * The \"generic\" operations
genericLength           :: [a] -> I
genericTake             :: I -> [a] -> [a]
genericDrop             :: I -> [a] -> [a]
genericIndex            :: [a] -> I -> a
genericSplitAt          :: I -> [a] -> ([a], [a])
genericReplicate        :: I -> a -> [a]

s = Stream.stream
u = Stream.unstream

-- * Basic interface
cons            = \x xs  -> u $ Stream.cons   x (s xs)
snoc            = \xs x  -> u $ Stream.snoc   (s xs) x
append          = \xs ys -> u $ Stream.append (s xs) (s ys)
head            = \xs    ->     Stream.head   (s xs)
last            = \xs    ->     Stream.last   (s xs)
tail            = \xs    -> u $ Stream.tail   (s xs)
init            = \xs    -> u $ Stream.init   (s xs)
null            = \xs    ->     Stream.null   (s xs)
length          = \xs    ->     Stream.length (s xs)


-- * List transformations
map             = \f   xs -> u $ Stream.map f (s xs)
--reverse               = Stream.reverse
intersperse     = \sep xs -> u $ Stream.intersperse sep (s xs)
intercalate     = \sep xs ->     Stream.concat (Stream.intersperse sep(s xs))

--transpose     = Stream.transpose

-- * Reducing lists (folds)
foldl           = \f z xs -> Stream.foldl   f z (s xs)
foldl'          = \f z xs -> Stream.foldl'  f z (s xs)
foldl1          = \f   xs -> Stream.foldl1  f   (s xs)
foldl1'         = \f   xs -> Stream.foldl1' f   (s xs)
foldr           = \f z xs -> Stream.foldr   f z (s xs)
foldr1          = \f   xs -> Stream.foldr1  f   (s xs)

-- ** Special folds
concat          = \  xs -> Stream.concat (s xs)
    -- concatMap :: (a -> Stream b) -> Stream a -> Stream b
concatMap       = \f xs -> u $ Stream.concatMap (s . f)  (s xs)
and             = \  xs -> Stream.and   (s xs)
or              = \  xs -> Stream.or    (s xs)
any             = \f xs -> Stream.any f (s xs)
all             = \f xs -> Stream.all f (s xs)
sum             = \  xs -> Stream.sum   (s xs)
product         = \  xs -> Stream.product   (s xs)
maximum         = \  xs -> Stream.maximum   (s xs)
{-# INLINE maximum #-}
minimum         = \  xs -> Stream.minimum   (s xs)
{-# INLINE minimum #-}

-- * Building lists
-- ** Scans
scanl           = \f z xs -> u (Stream.scanl f z (Stream.snoc (s xs) bottom))
 where
    bottom :: a
    bottom = error "StreamProperties.bottom"

scanl1          = \f xs -> u (Stream.scanl1 f (Stream.snoc (s xs) bottom))
 where
    bottom :: a
    bottom = error "StreamProperties.bottom"

{-
scanr           = \f z xs -> u (Stream.scanr f z (Stream.cons bottom (s xs)))
 where
    bottom :: a
    bottom = error "StreamProperties.bottom"
-}

{-
scanr1          = Stream.scanr1

-- ** Accumulating maps
mapAccumL       = Stream.mapAccumL
mapAccumR       = Stream.mapAccumR
-}
-- ** Infinite lists
iterate         = \f x -> u $ Stream.iterate   f x
repeat          = \  x -> u $ Stream.repeat      x
replicate       = \n x -> u $ Stream.replicate n x
cycle           = \ xs -> u $ Stream.cycle    (s xs)

-- ** Unfolding
unfoldr         = \f x -> u $ Stream.unfoldr f x

-- * Sublists
-- ** Extracting sublists
take            = \n xs -> u $ Stream.take    n (s xs)
drop            = \n xs -> u $ Stream.drop    n (s xs)
splitAt         = \n xs ->     Stream.splitAt n (s xs)
takeWhile       = \f xs -> u $ Stream.takeWhile f (s xs)
dropWhile       = \f xs -> u $ Stream.dropWhile f (s xs)
{-
span          = Stream.span
break           = Stream.break
group           = Stream.group
inits           = Stream.inits
tails           = Stream.tails
-}

-- * Predicates
isPrefixOf      = \xs ys -> Stream.isPrefixOf (s xs) (s ys)
{-
isSuffixOf      = Stream.isSuffixOf
isInfixOf       = Stream.isInfixOf
-}
-- * Searching lists
-- ** Searching by equality
elem            = \key xs -> Stream.elem   key (s xs)
--notElem               = Stream.notElem
lookup          = \key xs -> Stream.lookup key (s xs)

-- ** Searching with a predicate
find            = \p xs ->     Stream.find   p (s xs)
filter          = \p xs -> u $ Stream.filter p (s xs)
--partition     = Stream.partition

-- * Indexing lists
(!!)            = \xs n -> Stream.index (Stream.stream xs) n
--Wirdness: Stream.index needs to be eta-expanded and fully applied for the
-- code to be any good
findIndex       = \f xs -> Stream.findIndex f (s xs)
findIndices     = \p xs -> u (Stream.findIndices p (s xs))
elemIndex       = \x xs -> Stream.findIndex (x==) (s xs)
elemIndices     = \x xs -> u (Stream.findIndices (x==) (s xs))
{-# INLINE elemIndex #-}
{-# INLINE elemIndices #-}


-- * Zipping and unzipping lists
zip  = \xs ys    -> u (Stream.zip (s xs) (s ys))
zip3 = \xs ys zs -> u (Stream.zip3 (s xs) (s ys) (s zs))

zipWith         = \f xs ys    -> u (Stream.zipWith f (s xs) (s ys))
zipWith3        = \f xs ys zs -> u (Stream.zipWith3 f (s xs) (s ys) (s zs))

unzip           = Stream.unzip . s

{-
zip4            = Stream.zip4
zip5            = Stream.zip5
zip6            = Stream.zip6
zip7            = Stream.zip7
zipWith4        = Stream.zipWith4
zipWith5        = Stream.zipWith5
zipWith6        = Stream.zipWith6
zipWith7        = Stream.zipWith7
unzip3          = Stream.unzip3
unzip4          = Stream.unzip4
unzip5          = Stream.unzip5
unzip6          = Stream.unzip6
unzip7          = Stream.unzip7
-}

-- * Special lists
-- ** Functions on strings
{-
unlines         = \xs -> Stream.concatMap (\x -> x ++ ['\n']) (s xs)
lines           = \xs -> u (Stream.lines (s xs))
-}

{-
words           = Stream.words
unwords         = Stream.unwords

-- ** \"Set\" operations
nub             = Stream.nub
delete          = Stream.delete
(\\)            = (Stream.\\)
union           = Stream.union
intersect       = Stream.intersect

-- ** Ordered lists 
sort            = Stream.sort
insert          = Stream.insert

-- * Generalized functions
-- ** The \"By\" operations
-- *** User-supplied equality (replacing an Eq context)
nubBy           = Stream.nubBy
deleteBy        = Stream.deleteBy
deleteFirstsBy  = Stream.deleteFirstsBy
unionBy         = Stream.unionBy
intersectBy     = Stream.intersectBy
groupBy         = Stream.groupBy
-}

-- *** User-supplied comparison (replacing an Ord context)
{-
sortBy          = Stream.sortBy
-}
insertBy        = \cmp x xs -> u $ Stream.insertBy cmp x (s xs)

maximumBy       = \cmp xs -> Stream.maximumBy cmp (s xs)
minimumBy       = \cmp xs -> Stream.minimumBy cmp (s xs)

-- * The \"generic\" operations
genericLength           = \xs -> Stream.genericLength (s xs)
genericTake             = \n xs -> u $ Stream.genericTake n (s xs)
genericDrop             = \n xs -> u $ Stream.genericDrop n (s xs)
genericIndex            = \xs n -> Stream.genericIndex (s xs) n
genericSplitAt          = \n xs -> Stream.genericSplitAt n (s xs)
genericReplicate        = \n x  -> genericTake n (Prelude.repeat x)
