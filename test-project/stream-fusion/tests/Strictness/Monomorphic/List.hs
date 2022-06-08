module Strictness.Monomorphic.List where

--
-- just test the List api
--

import Strictness.Utils

import qualified Data.List.Stream as List

-- * Basic interface
(++)            :: [A] -> [A] -> [A]
head            :: [A] -> A
last            :: [A] -> A
tail            :: [A] -> [A]
init            :: [A] -> [A]
null            :: [A] -> Bool
length          :: [A] -> Int

-- * List transformations
map             :: (A -> B) -> [A] -> [B]
reverse         :: [A] -> [A]
intersperse     :: A -> [A] -> [A]
intercalate     :: [A] -> [[A]] -> [A]
transpose       :: [[A]] -> [[A]]

-- * Reducing lists (folds)
foldl           :: (B -> A -> B) -> B -> [A] -> B
foldl'          :: (B -> A -> B) -> B -> [A] -> B
foldl1          :: (A -> A -> A) -> [A] -> A
foldl1'         :: (A -> A -> A) -> [A] -> A
foldr           :: (A -> B -> B) -> B -> [A] -> B
foldr1          :: (A -> A -> A) -> [A] -> A

-- ** Special folds
concat          :: [[A]] -> [A]
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
scanr           :: (A -> B -> B) -> B -> [A] -> [B]
scanr1          :: (A -> A -> A) -> [A] -> [A]

-- ** Accumulating maps
mapAccumL       :: (C -> A -> (C, B)) -> C -> [A] -> (C, [B])
mapAccumR       :: (C -> A -> (C, B)) -> C -> [A] -> (C, [B])

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
span            :: (A -> Bool) -> [A] -> ([A], [A])
break           :: (A -> Bool) -> [A] -> ([A], [A])
group           :: [A] -> [[A]]
inits           :: [A] -> [[A]]
tails           :: [A] -> [[A]]

-- * Predicates
isPrefixOf      :: [A] -> [A] -> Bool
isSuffixOf      :: [A] -> [A] -> Bool
isInfixOf       :: [A] -> [A] -> Bool

-- * Searching lists
-- ** Searching by equality
elem            :: A -> [A] -> Bool
notElem         :: A -> [A] -> Bool
lookup          :: A -> [(A, B)] -> Maybe B

-- ** Searching with A predicate
find            :: (A -> Bool) -> [A] -> Maybe A
filter          :: (A -> Bool) -> [A] -> [A]
partition       :: (A -> Bool) -> [A] -> ([A], [A])

-- * Indexing lists
(!!)            :: [A] -> Int -> A
elemIndex       :: A -> [A] -> Maybe Int
elemIndices     :: A -> [A] -> [Int]
findIndex       :: (A -> Bool) -> [A] -> Maybe Int
findIndices     :: (A -> Bool) -> [A] -> [Int]

-- * Zipping and unzipping lists
zip             :: [A] -> [B] -> [(A, B)]
zip3            :: [A] -> [B] -> [C] -> [(A, B, C)]
zip4            :: [A] -> [B] -> [C] -> [D] -> [(A, B, C, D)]
zip5            :: [A] -> [B] -> [C] -> [D] -> [E] -> [(A, B, C, D, E)]
zip6            :: [A] -> [B] -> [C] -> [D] -> [E] -> [F] -> [(A, B, C, D, E, F)]
zip7            :: [A] -> [B] -> [C] -> [D] -> [E] -> [F] -> [G] -> [(A, B, C, D, E, F, G)]
zipWith         :: (A -> B -> C) -> [A] -> [B] -> [C]
zipWith3        :: (A -> B -> C -> D) -> [A] -> [B] -> [C] -> [D]
zipWith4        :: (A -> B -> C -> D -> E) -> [A] -> [B] -> [C] -> [D] -> [E]
zipWith5        :: (A -> B -> C -> D -> E -> F) -> [A] -> [B] -> [C] -> [D] -> [E] -> [F]
zipWith6        :: (A -> B -> C -> D -> E -> F -> G) -> [A] -> [B] -> [C] -> [D] -> [E] -> [F] -> [G]
zipWith7        :: (A -> B -> C -> D -> E -> F -> G -> H) -> [A] -> [B] -> [C] -> [D] -> [E] -> [F] -> [G] -> [H]
unzip           :: [(A, B)] -> ([A], [B])
unzip3          :: [(A, B, C)] -> ([A], [B], [C])
unzip4          :: [(A, B, C, D)] -> ([A], [B], [C], [D])
unzip5          :: [(A, B, C, D, E)] -> ([A], [B], [C], [D], [E])
unzip6          :: [(A, B, C, D, E, F)] -> ([A], [B], [C], [D], [E], [F])
unzip7          :: [(A, B, C, D, E, F, G)] -> ([A], [B], [C], [D], [E], [F], [G])

-- * Special lists
-- ** Functions on strings
lines           :: String -> [String]
words           :: String -> [String]
unlines         :: [String] -> String
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

-- *** User-supplied comparison (replacing an Ord context)
sortBy          :: (A -> A -> Ordering) -> [A] -> [A]       --fixme: need to use Bool domain for order
insertBy        :: (A -> A -> Ordering) -> A -> [A] -> [A]
maximumBy       :: (A -> A -> Ordering) -> [A] -> A
minimumBy       :: (A -> A -> Ordering) -> [A] -> A

-- * The \"generic\" operations
genericLength           :: [A] -> I
genericTake             :: I -> [A] -> [A]
genericDrop             :: I -> [A] -> [A]
genericSplitAt          :: I -> [A] -> ([A], [A])
genericIndex            :: [A] -> I -> A
genericReplicate        :: I -> A -> [A]



-- * Basic interface
(++)            = (List.++)
head            = List.head
last            = List.last
tail            = List.tail
init            = List.init
null            = List.null
length          = List.length

-- * List transformations
map             = List.map
reverse         = List.reverse
intersperse     = List.intersperse
intercalate     = List.intercalate
transpose       = List.transpose

-- * Reducing lists (folds)
foldl           = List.foldl
foldl'          = List.foldl'
foldl1          = List.foldl1
foldl1'         = List.foldl1'
foldr           = List.foldr
foldr1          = List.foldr1

-- ** Special folds
concat          = List.concat
concatMap       = List.concatMap
and             = List.and
or              = List.or
any             = List.any
all             = List.all
sum             = List.sum
product         = List.product
maximum         = List.maximum
minimum         = List.minimum

-- * Building lists
-- ** Scans
scanl           = List.scanl
scanl1          = List.scanl1
scanr           = List.scanr
scanr1          = List.scanr1

-- ** Accumulating maps
mapAccumL       = List.mapAccumL
mapAccumR       = List.mapAccumR

-- ** Infinite lists
iterate         = List.iterate
repeat          = List.repeat
replicate       = List.replicate
cycle           = List.cycle

-- ** Unfolding
unfoldr         = List.unfoldr

-- * Sublists
-- ** Extracting sublists
take            = List.take
drop            = List.drop
splitAt         = List.splitAt
takeWhile       = List.takeWhile
dropWhile       = List.dropWhile
span            = List.span
break           = List.break
group           = List.group
inits           = List.inits
tails           = List.tails

-- * Predicates
isPrefixOf      = List.isPrefixOf
isSuffixOf      = List.isSuffixOf
isInfixOf       = List.isInfixOf

-- * Searching lists
-- ** Searching by equality
elem            = List.elem
notElem         = List.notElem
lookup          = List.lookup

-- ** Searching with a predicate
find            = List.find
filter          = List.filter
partition       = List.partition

-- * Indexing lists
(!!)            = (List.!!)
elemIndex       = List.elemIndex
elemIndices     = List.elemIndices
findIndex       = List.findIndex
findIndices     = List.findIndices

-- * Zipping and unzipping lists
zip             = List.zip
zip3            = List.zip3
zip4            = List.zip4
zip5            = List.zip5
zip6            = List.zip6
zip7            = List.zip7
zipWith         = List.zipWith
zipWith3        = List.zipWith3
zipWith4        = List.zipWith4
zipWith5        = List.zipWith5
zipWith6        = List.zipWith6
zipWith7        = List.zipWith7
unzip           = List.unzip
unzip3          = List.unzip3
unzip4          = List.unzip4
unzip5          = List.unzip5
unzip6          = List.unzip6
unzip7          = List.unzip7

-- * Special lists
-- ** Functions on strings
lines           = List.lines
words           = List.words
unlines         = List.unlines
unwords         = List.unwords

-- ** \"Set\" operations
nub             = List.nub
delete          = List.delete
(\\)            = (List.\\)
union           = List.union
intersect       = List.intersect

-- ** Ordered lists 
sort            = List.sort
insert          = List.insert

-- * Generalized functions
-- ** The \"By\" operations
-- *** User-supplied equality (replacing an Eq context)
nubBy           = List.nubBy
deleteBy        = List.deleteBy
deleteFirstsBy  = List.deleteFirstsBy
unionBy         = List.unionBy
intersectBy     = List.intersectBy
groupBy         = List.groupBy

-- *** User-supplied comparison (replacing an Ord context)
sortBy          = List.sortBy
insertBy        = List.insertBy
maximumBy       = List.maximumBy
minimumBy       = List.minimumBy

-- * The \"generic\" operations
genericLength           = List.genericLength
genericTake             = List.genericTake
genericDrop             = List.genericDrop
genericSplitAt          = List.genericSplitAt
genericIndex            = List.genericIndex
genericReplicate        = List.genericReplicate
