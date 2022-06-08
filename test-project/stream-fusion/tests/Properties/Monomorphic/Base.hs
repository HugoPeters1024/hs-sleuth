--
-- The Data.List api
--
module Properties.Monomorphic.Base where

import Properties.Utils

import qualified Data.List as Spec

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
sortBy          :: (A -> A -> Ordering) -> [A] -> [A]
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
(++)            = (Spec.++)
head            = Spec.head
last            = Spec.last
tail            = Spec.tail
init            = Spec.init
null            = Spec.null
length          = Spec.length

-- * List transformations
map             = Spec.map
reverse         = Spec.reverse
intersperse     = Spec.intersperse

-- intercalate     = -- Spec.intercalate
intercalate xs xss = Spec.concat (Spec.intersperse xs xss)

transpose       = Spec.transpose

-- * Reducing lists (folds)
foldl           = Spec.foldl
foldl'          = Spec.foldl'
foldl1          = Spec.foldl1
foldl1'         = Spec.foldl1'
foldr           = Spec.foldr
foldr1          = Spec.foldr1

-- ** Special folds
concat          = Spec.concat
concatMap       = Spec.concatMap
and             = Spec.and
or              = Spec.or
any             = Spec.any
all             = Spec.all
sum             = Spec.sum
product         = Spec.product
maximum         = Spec.maximum
minimum         = Spec.minimum

-- * Building lists
-- ** Scans
scanl           = Spec.scanl
scanl1          = Spec.scanl1
scanr           = Spec.scanr
scanr1          = Spec.scanr1

-- ** Accumulating maps
mapAccumL       = Spec.mapAccumL
mapAccumR       = Spec.mapAccumR

-- ** Infinite lists
iterate         = Spec.iterate
repeat          = Spec.repeat
replicate       = Spec.replicate
cycle           = Spec.cycle

-- ** Unfolding
unfoldr         = Spec.unfoldr

-- * Sublists
-- ** Extracting sublists
take            = Spec.take
drop            = Spec.drop
splitAt         = Spec.splitAt
takeWhile       = Spec.takeWhile
dropWhile       = Spec.dropWhile
span            = Spec.span
break           = Spec.break
group           = Spec.group
inits           = Spec.inits
tails           = Spec.tails

-- * Predicates
isPrefixOf      = Spec.isPrefixOf
isSuffixOf      = Spec.isSuffixOf
isInfixOf       = Spec.isInfixOf

-- * Searching lists
-- ** Searching by equality
elem            = Spec.elem
notElem         = Spec.notElem
lookup          = Spec.lookup

-- ** Searching with a predicate
find            = Spec.find
filter          = Spec.filter
partition       = Spec.partition

-- * Indexing lists
(!!)            = (Spec.!!)
elemIndex       = Spec.elemIndex
elemIndices     = Spec.elemIndices
findIndex       = Spec.findIndex
findIndices     = Spec.findIndices

-- * Zipping and unzipping lists
zip             = Spec.zip
zip3            = Spec.zip3
zip4            = Spec.zip4
zip5            = Spec.zip5
zip6            = Spec.zip6
zip7            = Spec.zip7
zipWith         = Spec.zipWith
zipWith3        = Spec.zipWith3
zipWith4        = Spec.zipWith4
zipWith5        = Spec.zipWith5
zipWith6        = Spec.zipWith6
zipWith7        = Spec.zipWith7
unzip           = Spec.unzip
unzip3          = Spec.unzip3
unzip4          = Spec.unzip4
unzip5          = Spec.unzip5
unzip6          = Spec.unzip6
unzip7          = Spec.unzip7

-- * Special lists
-- ** Functions on strings
lines           = Spec.lines
words           = Spec.words
unlines         = Spec.unlines
unwords         = Spec.unwords

-- ** \"Set\" operations
nub             = Spec.nub
delete          = Spec.delete
(\\)            = (Spec.\\)
union           = Spec.union
intersect       = Spec.intersect

-- ** Ordered lists 
sort            = Spec.sort
insert          = Spec.insert

-- * Generalized functions
-- ** The \"By\" operations
-- *** User-supplied equality (replacing an Eq context)
nubBy           = Spec.nubBy
deleteBy        = Spec.deleteBy
deleteFirstsBy  = Spec.deleteFirstsBy
unionBy         = Spec.unionBy
intersectBy     = Spec.intersectBy
groupBy         = Spec.groupBy

-- *** User-supplied comparison (replacing an Ord context)
sortBy          = Spec.sortBy
insertBy        = Spec.insertBy
maximumBy       = Spec.maximumBy
minimumBy       = Spec.minimumBy

-- * The \"generic\" operations
genericLength           = Spec.genericLength
genericTake             = Spec.genericTake
genericDrop             = Spec.genericDrop
genericSplitAt          = Spec.genericSplitAt
genericIndex            = Spec.genericIndex
genericReplicate        = Spec.genericReplicate
