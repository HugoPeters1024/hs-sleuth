--
-- raw stream interface, no list wrappers
--
-- These specify the versions used when fusion occurs.
--
-- So we can check our stream implementations, which are only used when
-- fusion happens, are correct.
--

module Properties.Monomorphic.Stream where

import Prelude 
import qualified Prelude 

import Properties.Utils

import qualified Data.Stream as Stream
import Data.Stream (Stream)


-- * Basic interface
cons            :: A -> Stream A -> Stream A
snoc            :: Stream A -> A -> Stream A
append          :: Stream A -> Stream A -> Stream A
head            :: Stream A -> A
last            :: Stream A -> A
tail            :: Stream A -> Stream A
init            :: Stream A -> Stream A
null            :: Stream A -> Bool
length          :: Stream A -> Int

-- * List transformations
map             :: (A -> B) -> Stream A -> Stream B
--reverse         :: Stream A -> Stream A
intersperse     :: A -> Stream A -> Stream A
--intercalate     :: Stream A -> Stream (Stream A) -> Stream A
--transpose       :: Stream (Stream A) -> Stream (Stream A)

-- * Reducing lists (folds)
foldl           :: (B -> A -> B) -> B -> Stream A -> B
foldl'          :: (B -> A -> B) -> B -> Stream A -> B
foldl1          :: (A -> A -> A) -> Stream A -> A
foldl1'         :: (A -> A -> A) -> Stream A -> A
foldr           :: (A -> B -> B) -> B -> Stream A -> B
foldr1          :: (A -> A -> A) -> Stream A -> A

-- ** Special folds
-- concat          :: Stream (Stream A) -> Stream A
concatMap       :: (A -> Stream B) -> Stream A -> Stream B
and             :: Stream Bool -> Bool
or              :: Stream Bool -> Bool
any             :: (A -> Bool) -> Stream A -> Bool
all             :: (A -> Bool) -> Stream A -> Bool
sum             :: Stream N -> N
product         :: Stream N -> N
maximum         :: Stream OrdA -> OrdA
minimum         :: Stream OrdA -> OrdA

-- * Building lists
-- ** Scans
scanl           :: (A -> B -> A) -> A -> Stream B -> Stream A
scanl1          :: (A -> A -> A) -> Stream A -> Stream A
--scanr           :: (A -> B -> B) -> B -> Stream A -> Stream B
--scanr1          :: (A -> A -> A) -> Stream A -> Stream A

-- ** Accumulating maps
--mapAccumL       :: (C -> A -> (C, B)) -> C -> Stream A -> (C, Stream B)
--mapAccumR       :: (C -> A -> (C, B)) -> C -> Stream A -> (C, Stream B)

-- ** Infinite lists
iterate         :: (A -> A) -> A -> Stream A
repeat          :: A -> Stream A
replicate       :: Int -> A -> Stream A
cycle           :: Stream A -> Stream A

-- ** Unfolding
unfoldr         :: (B -> Maybe (A, B)) -> B -> Stream A

-- * Sublists
-- ** Extracting sublists
take            :: Int -> Stream A -> Stream A
drop            :: Int -> Stream A -> Stream A
splitAt         :: Int -> Stream A -> ([A], [A])
takeWhile       :: (A -> Bool) -> Stream A -> Stream A
dropWhile       :: (A -> Bool) -> Stream A -> Stream A
--span            :: (A -> Bool) -> Stream A -> (Stream A, Stream A)
--break           :: (A -> Bool) -> Stream A -> (Stream A, Stream A)
--group           :: Stream A -> Stream (Stream A)
--inits           :: Stream A -> Stream (Stream A)
--tails           :: Stream A -> Stream (Stream A)

-- * Predicates
isPrefixOf      :: Stream A -> Stream A -> Bool
--isSuffixOf      :: Stream A -> Stream A -> Bool
--isInfixOf       :: Stream A -> Stream A -> Bool

-- * Searching lists
-- ** Searching by equality
elem            :: A -> Stream A -> Bool
--notElem         :: A -> Stream A -> Bool
lookup          :: A -> Stream (A, B) -> Maybe B

-- ** Searching with A predicate
find            :: (A -> Bool) -> Stream A -> Maybe A
filter          :: (A -> Bool) -> Stream A -> Stream A
--partition       :: (A -> Bool) -> Stream A -> (Stream A, Stream A)

-- * Indexing lists
(!!)            :: Stream A -> Int -> A
elemIndex       :: A -> Stream A -> Maybe Int
findIndex       :: (A -> Bool) -> Stream A -> Maybe Int
elemIndices     :: A -> Stream A -> Stream Int
findIndices     :: (A -> Bool) -> Stream A -> Stream Int

-- * Zipping and unzipping lists
zip             :: Stream A -> Stream B -> Stream (A, B)
zip3            :: Stream A -> Stream B -> Stream C -> Stream (A, B, C)

--zip4            :: Stream A -> Stream B -> Stream C -> Stream D -> Stream (A, B, C, D)
--zip5            :: Stream A -> Stream B -> Stream C -> Stream D -> Stream E -> Stream (A, B, C, D, E)
--zip6            :: Stream A -> Stream B -> Stream C -> Stream D -> Stream E -> Stream F -> Stream (A, B, C, D, E, F)
--zip7            :: Stream A -> Stream B -> Stream C -> Stream D -> Stream E -> Stream F -> Stream G -> Stream (A, B, C, D, E, F, G)

zipWith         :: (A -> B -> C) -> Stream A -> Stream B -> Stream C
zipWith3        :: (A -> B -> C -> D) -> Stream A -> Stream B -> Stream C -> Stream D

--zipWith4        :: (A -> B -> C -> D -> E) -> Stream A -> Stream B -> Stream C -> Stream D -> Stream E
--zipWith5        :: (A -> B -> C -> D -> E -> F) -> Stream A -> Stream B -> Stream C -> Stream D -> Stream E -> Stream F
--zipWith6        :: (A -> B -> C -> D -> E -> F -> G) -> Stream A -> Stream B -> Stream C -> Stream D -> Stream E -> Stream F -> Stream G
--zipWith7        :: (A -> B -> C -> D -> E -> F -> G -> H) -> Stream A -> Stream B -> Stream C -> Stream D -> Stream E -> Stream F -> Stream G -> Stream H

unzip           :: Stream (A, B) -> ([A], [B])

{-
--unzip3          :: Stream (A, B, C) -> (Stream A, Stream B, Stream C)
--unzip4          :: Stream (A, B, C, D) -> (Stream A, Stream B, Stream C, Stream D)
--unzip5          :: Stream (A, B, C, D, E) -> (Stream A, Stream B, Stream C, Stream D, Stream E)
--unzip6          :: Stream (A, B, C, D, E, F) -> (Stream A, Stream B, Stream C, Stream D, Stream E, Stream F)
--unzip7          :: Stream (A, B, C, D, E, F, G) -> (Stream A, Stream B, Stream C, Stream D, Stream E, Stream F, Stream G)
-}

-- * Special lists
-- ** Functions on strings
--lines           :: Stream Char -> Stream [Char]


--words           :: String -> Stream String
--unlines         :: Stream String -> String
--unwords         :: Stream String -> String

-- ** \"Set\" operations
--nub             :: Stream A -> Stream A
--delete          :: A -> Stream A -> Stream A
--(\\)            :: Stream A -> Stream A -> Stream A
--union           :: Stream A -> Stream A -> Stream A
--intersect       :: Stream A -> Stream A -> Stream A

-- ** Ordered lists 
--sort            :: Stream OrdA -> Stream OrdA
--insert          :: OrdA -> Stream OrdA -> Stream OrdA

-- * Generalized functions
-- ** The \"By\" operations
-- *** User-supplied equality (replacing an Eq context)
--nubBy           :: (A -> A -> Bool) -> Stream A -> Stream A
--deleteBy        :: (A -> A -> Bool) -> A -> Stream A -> Stream A
--deleteFirstsBy  :: (A -> A -> Bool) -> Stream A -> Stream A -> Stream A
--unionBy         :: (A -> A -> Bool) -> Stream A -> Stream A -> Stream A
--intersectBy     :: (A -> A -> Bool) -> Stream A -> Stream A -> Stream A
--groupBy         :: (A -> A -> Bool) -> Stream A -> Stream (Stream A)

-- *** User-supplied comparison (replacing an Ord context)
--sortBy          :: (A -> A -> Ordering) -> Stream A -> Stream A

insertBy        :: (A -> A -> Ordering) -> A -> Stream A -> Stream A
maximumBy       :: (A -> A -> Ordering) -> Stream A -> A
minimumBy       :: (A -> A -> Ordering) -> Stream A -> A

-- * The \"generic\" operations

genericLength           :: Stream A -> I
genericTake             :: I -> Stream A -> Stream A
genericDrop             :: I -> Stream A -> Stream A
genericSplitAt          :: I -> Stream A -> ([A], [A])
genericIndex            :: Stream A -> I -> A

--genericReplicate        :: I -> A -> Stream A

-- * Basic interface
cons            = Stream.cons
snoc            = Stream.snoc
append          = Stream.append
head            = Stream.head
last            = Stream.last
tail            = Stream.tail
init            = Stream.init
null            = Stream.null
length          = Stream.length


-- * List transformations
map             = Stream.map
--reverse       = Stream.reverse
intersperse     = Stream.intersperse
--intercalate   = Stream.intercalate
--transpose     = Stream.transpose

-- * Reducing lists (folds)
foldl           = Stream.foldl
foldl'          = Stream.foldl'
foldl1          = Stream.foldl1
foldl1'         = Stream.foldl1'
foldr           = Stream.foldr
foldr1          = Stream.foldr1

-- ** Special folds
-- concat          = Stream.concat
concatMap       = Stream.concatMap
and             = Stream.and
or              = Stream.or
any             = Stream.any
all             = Stream.all
sum             = Stream.sum
product         = Stream.product
maximum         = Stream.maximum
minimum         = Stream.minimum

-- * Building lists
-- ** Scans
scanl           = \f z xs -> Stream.scanl f z (Stream.snoc xs bottom)
  where
     bottom :: a
     bottom = error "bottom"

scanl1          = \f xs    -> Stream.scanl1 f (Stream.snoc xs bottom)
  where
     bottom :: a
     bottom = error "bottom"

{-
scanr           = Stream.scanr
scanr1          = Stream.scanr1

-- ** Accumulating maps
mapAccumL       = Stream.mapAccumL
mapAccumR       = Stream.mapAccumR
-}
-- ** Infinite lists
iterate         = Stream.iterate
repeat          = Stream.repeat
replicate       = Stream.replicate
cycle           = Stream.cycle

-- ** Unfolding
unfoldr         = Stream.unfoldr

-- * Sublists
-- ** Extracting sublists
take            = Stream.take
drop            = Stream.drop
splitAt         = Stream.splitAt
takeWhile       = Stream.takeWhile
dropWhile       = Stream.dropWhile
{-
span          = Stream.span
break           = Stream.break
group           = Stream.group
inits           = Stream.inits
tails           = Stream.tails
-}

-- * Predicates
isPrefixOf      = Stream.isPrefixOf
{-
isSuffixOf      = Stream.isSuffixOf
isInfixOf       = Stream.isInfixOf
-}
-- * Searching lists
-- ** Searching by equality
elem            = Stream.elem
--notElem       = Stream.notElem
lookup          = Stream.lookup

-- ** Searching with a predicate
find            = Stream.find
filter          = Stream.filter
--partition     = Stream.partition

-- * Indexing lists
(!!)            = Stream.index
findIndex       = Stream.findIndex
elemIndex       = Stream.elemIndex
elemIndices     = Stream.elemIndices
findIndices     = Stream.findIndices

-- * Zipping and unzipping lists
zip             = Stream.zip
zip3            = Stream.zip3
--zip4            = Stream.zip4
--zip5            = Stream.zip5
--zip6            = Stream.zip6
--zip7            = Stream.zip7
zipWith         = Stream.zipWith
zipWith3        = Stream.zipWith3
--zipWith4        = Stream.zipWith4
--zipWith5        = Stream.zipWith5
--zipWith6        = Stream.zipWith6
--zipWith7        = Stream.zipWith7
unzip           = Stream.unzip
--unzip3          = Stream.unzip3
--unzip4          = Stream.unzip4
--unzip5          = Stream.unzip5
--unzip6          = Stream.unzip6
--unzip7          = Stream.unzip7

-- * Special lists
-- ** Functions on strings

{-
lines           = Stream.lines
words           = Stream.words
unlines         = Stream.unlines
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
insertBy        = Stream.insertBy
{-
sortBy          = Stream.sortBy
-}

maximumBy       = Stream.maximumBy
minimumBy       = Stream.minimumBy

-- * The \"generic\" operations
genericLength           = Stream.genericLength
genericTake             = Stream.genericTake
genericDrop             = Stream.genericDrop
genericIndex            = Stream.genericIndex
genericSplitAt          = Stream.genericSplitAt
--genericReplicate        = Stream.genericReplicate
