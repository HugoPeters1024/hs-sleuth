--
-- list like wrappers for abstract streams
--
-- These specify the versions used when fusion occurs.
--
-- So we can check our stream implementations, which are only used when
-- fusion happens, are correct.
--

module Properties.Monomorphic.StreamList where

import Prelude
import qualified Prelude 
import Properties.Utils

import qualified Data.Stream as Stream

-- * Basic interface
cons            :: A -> [A] -> [A]
snoc            :: [A] -> A -> [A]
append          :: [A] -> [A] -> [A]
head            :: [A] -> A
last            :: [A] -> A
tail            :: [A] -> [A]
init            :: [A] -> [A]
null            :: [A] -> Bool
length          :: [A] -> Int


-- * List transformations
map             :: (A -> B) -> [A] -> [B]
--reverse       :: [A] -> [A]
intersperse     :: A -> [A] -> [A]
intercalate   :: [A] -> [[A]] -> [A]
--transpose     :: [[A]] -> [[A]]

-- * Reducing lists (folds)
foldl           :: (B -> A -> B) -> B -> [A] -> B
foldl'          :: (B -> A -> B) -> B -> [A] -> B
foldl1          :: (A -> A -> A) -> [A] -> A
foldl1'         :: (A -> A -> A) -> [A] -> A
foldr           :: (A -> B -> B) -> B -> [A] -> B
foldr1          :: (A -> A -> A) -> [A] -> A

-- ** Special folds
--concat          :: [[A]] -> [A]
concatMap       :: (A -> [B]) -> [A] -> [B]
and             :: [Bool] -> Bool
or              :: [Bool] -> Bool
any             :: (A -> Bool) -> [A] -> Bool
all             :: (A -> Bool) -> [A] -> Bool
sum             :: [N] -> N
product         :: [N] -> N
maximum         :: [OrdA] -> OrdA
minimum         :: [OrdA] -> OrdA

-- * Building lists
-- ** Scans
scanl           :: (A -> B -> A) -> A -> [B] -> [A]
scanl1          :: (A -> A -> A) -> [A] -> [A]

{-
scanr           :: (A -> B -> B) -> B -> [A] -> [B]
scanr1          :: (A -> A -> A) -> [A] -> [A]

-- ** Accumulating maps
mapAccumL       :: (C -> A -> (C, B)) -> C -> [A] -> (C, [B])
mapAccumR       :: (C -> A -> (C, B)) -> C -> [A] -> (C, [B])
-}

-- ** Infinite lists
iterate         :: (A -> A) -> A -> [A]
repeat          :: A -> [A]
replicate       :: Int -> A -> [A]
cycle           :: [A] -> [A]

-- ** Unfolding
unfoldr         :: (B -> Maybe (A, B)) -> B -> [A]

-- * Sublists
-- ** Extracting sublists
take            :: Int -> [A] -> [A]
drop            :: Int -> [A] -> [A]
splitAt         :: Int -> [A] -> ([A], [A])
takeWhile       :: (A -> Bool) -> [A] -> [A]
dropWhile       :: (A -> Bool) -> [A] -> [A]

{-
span          :: (A -> Bool) -> [A] -> ([A], [A])
break           :: (A -> Bool) -> [A] -> ([A], [A])
group           :: [A] -> [[A]]
inits           :: [A] -> [[A]]
tails           :: [A] -> [[A]]
-}

-- * Predicates
isPrefixOf      :: [A] -> [A] -> Bool
{-
isSuffixOf      :: [A] -> [A] -> Bool
isInfixOf       :: [A] -> [A] -> Bool
-}

-- * Searching lists
-- ** Searching by equality
elem            :: A -> [A] -> Bool
--notElem               :: A -> [A] -> Bool
lookup          :: A -> [(A, B)] -> Maybe B

-- ** Searching with A predicate
find            :: (A -> Bool) -> [A] -> Maybe A
filter          :: (A -> Bool) -> [A] -> [A]
--partition     :: (A -> Bool) -> [A] -> ([A], [A])

-- * Indexing lists
(!!)            :: [A] -> Int -> A
findIndex       :: (A -> Bool) -> [A] -> Maybe Int
elemIndex       :: A -> [A] -> Maybe Int
elemIndices     :: A -> [A] -> [Int]
findIndices     :: (A -> Bool) -> [A] -> [Int]

-- * Zipping and unzipping lists
zip             :: [A] -> [B] -> [(A, B)]
zip3            :: [A] -> [B] -> [C] -> [(A, B, C)]
{-
zip4            :: [A] -> [B] -> [C] -> [D] -> [(A, B, C, D)]
zip5            :: [A] -> [B] -> [C] -> [D] -> [E] -> [(A, B, C, D, E)]
zip6            :: [A] -> [B] -> [C] -> [D] -> [E] -> [F] -> [(A, B, C, D, E, F)]
zip7            :: [A] -> [B] -> [C] -> [D] -> [E] -> [F] -> [G] -> [(A, B, C, D, E, F, G)]
-}

zipWith         :: (A -> B -> C) -> [A] -> [B] -> [C]
zipWith3        :: (A -> B -> C -> D) -> [A] -> [B] -> [C] -> [D]
{-
zipWith4        :: (A -> B -> C -> D -> E) -> [A] -> [B] -> [C] -> [D] -> [E]
zipWith5        :: (A -> B -> C -> D -> E -> F) -> [A] -> [B] -> [C] -> [D] -> [E] -> [F]
zipWith6        :: (A -> B -> C -> D -> E -> F -> G) -> [A] -> [B] -> [C] -> [D] -> [E] -> [F] -> [G]
zipWith7        :: (A -> B -> C -> D -> E -> F -> G -> H) -> [A] -> [B] -> [C] -> [D] -> [E] -> [F] -> [G] -> [H]
-}

unzip           :: [(A, B)] -> ([A], [B])
{-
unzip3          :: [(A, B, C)] -> ([A], [B], [C])
unzip4          :: [(A, B, C, D)] -> ([A], [B], [C], [D])
unzip5          :: [(A, B, C, D, E)] -> ([A], [B], [C], [D], [E])
unzip6          :: [(A, B, C, D, E, F)] -> ([A], [B], [C], [D], [E], [F])
unzip7          :: [(A, B, C, D, E, F, G)] -> ([A], [B], [C], [D], [E], [F], [G])
-}

-- * Special lists
-- ** Functions on strings
unlines         :: [String] -> String
-- lines           :: String -> [String]
{-
words           :: String -> [String]
unwords         :: [String] -> String

-- ** \"Set\" operations
nub             :: [A] -> [A]
delete          :: A -> [A] -> [A]
(\\)            :: [A] -> [A] -> [A]
union           :: [A] -> [A] -> [A]
intersect       :: [A] -> [A] -> [A]

-- ** Ordered lists 
sort            :: [OrdA] -> [OrdA]
insert          :: OrdA -> [OrdA] -> [OrdA]

-- * Generalized functions
-- ** The \"By\" operations
-- *** User-supplied equality (replacing an Eq context)
nubBy           :: (A -> A -> Bool) -> [A] -> [A]
deleteBy        :: (A -> A -> Bool) -> A -> [A] -> [A]
deleteFirstsBy  :: (A -> A -> Bool) -> [A] -> [A] -> [A]
unionBy         :: (A -> A -> Bool) -> [A] -> [A] -> [A]
intersectBy     :: (A -> A -> Bool) -> [A] -> [A] -> [A]
groupBy         :: (A -> A -> Bool) -> [A] -> [[A]]
-}

-- *** User-supplied comparison (replacing an Ord context)
insertBy        :: (A -> A -> Ordering) -> A -> [A] -> [A]
{-
sortBy          :: (A -> A -> Ordering) -> [A] -> [A]
-}
maximumBy       :: (A -> A -> Ordering) -> [A] -> A
minimumBy       :: (A -> A -> Ordering) -> [A] -> A

-- * The \"generic\" operations
genericLength           :: [A] -> I
genericTake             :: I -> [A] -> [A]
genericDrop             :: I -> [A] -> [A]
genericIndex            :: [A] -> I -> A
genericSplitAt          :: I -> [A] -> ([A], [A])
genericReplicate        :: I -> A -> [A]

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
intercalate     = \sep xs -> u $ Stream.concatMap s (Stream.intersperse sep(s xs))

--transpose     = Stream.transpose

-- * Reducing lists (folds)
foldl           = \f z xs -> Stream.foldl   f z (s xs)
foldl'          = \f z xs -> Stream.foldl'  f z (s xs)
foldl1          = \f   xs -> Stream.foldl1  f   (s xs)
foldl1'         = \f   xs -> Stream.foldl1' f   (s xs)
foldr           = \f z xs -> Stream.foldr   f z (s xs)
foldr1          = \f   xs -> Stream.foldr1  f   (s xs)

-- ** Special folds
-- concat          = \  xs -> Stream.concat xs
concatMap       = \f xs -> u $ Stream.concatMap (s . f) (s xs)
and             = \  xs -> Stream.and   (s xs)
or              = \  xs -> Stream.or    (s xs)
any             = \f xs -> Stream.any f (s xs)
all             = \f xs -> Stream.all f (s xs)
sum             = \  xs -> Stream.sum   (s xs)
product         = \  xs -> Stream.product   (s xs)
maximum         = \  xs -> Stream.maximum   (s xs)
minimum         = \  xs -> Stream.minimum   (s xs)

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
(!!)            = \xs -> Stream.index (s xs)
findIndex       = \f xs -> Stream.findIndex f (s xs)
elemIndex       = \x xs -> Stream.elemIndex x (s xs)
elemIndices     = \x xs -> u (Stream.elemIndices x (s xs))
findIndices     = \p xs -> u (Stream.findIndices p (s xs))


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
unlines         = \xs -> u (Stream.concatMap (\x -> Stream.snoc (s x) '\n') (s xs))
-- lines           = \xs -> u (Stream.lines (s xs))

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

