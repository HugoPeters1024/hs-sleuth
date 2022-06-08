-- Haskell98 list spec wrapped in a stream interface so we can compare it to
-- the stream implementation and test using streams that contain Skips.

module Properties.Monomorphic.SpecStream where

import Properties.Utils

import qualified Spec.List as Spec
import qualified Spec.ListExts as Spec
import Data.Stream as Stream (Stream, stream, unstream)

-- * Basic interface
cons            :: A -> Stream A -> Stream A
snoc            :: Stream A -> A -> Stream A
(++)            :: Stream A -> Stream A -> Stream A
head            :: Stream A -> A
last            :: Stream A -> A
tail            :: Stream A -> Stream A
init            :: Stream A -> Stream A
null            :: Stream A -> Bool
length          :: Stream A -> Int

-- * List transformations
map             :: (A -> B) -> Stream A -> Stream B
reverse         :: Stream A -> Stream A
intersperse     :: A -> Stream A -> Stream A
intercalate     :: Stream A -> Stream (Stream A) -> Stream A
transpose       :: Stream (Stream A) -> Stream (Stream A)

-- * Reducing lists (folds)
foldl           :: (B -> A -> B) -> B -> Stream A -> B
foldl'          :: (B -> A -> B) -> B -> Stream A -> B
foldl1          :: (A -> A -> A) -> Stream A -> A
foldl1'         :: (A -> A -> A) -> Stream A -> A
foldr           :: (A -> B -> B) -> B -> Stream A -> B
foldr1          :: (A -> A -> A) -> Stream A -> A

-- ** Special folds
concat          :: Stream (Stream A) -> Stream A
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
scanr           :: (A -> B -> B) -> B -> Stream A -> Stream B
scanr1          :: (A -> A -> A) -> Stream A -> Stream A

-- ** Accumulating maps
mapAccumL       :: (C -> A -> (C, B)) -> C -> Stream A -> (C, Stream B)
mapAccumR       :: (C -> A -> (C, B)) -> C -> Stream A -> (C, Stream B)

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
span            :: (A -> Bool) -> Stream A -> ([A], [A])
break           :: (A -> Bool) -> Stream A -> ([A], [A])
group           :: Stream A -> Stream (Stream A)
inits           :: Stream A -> Stream (Stream A)
tails           :: Stream A -> Stream (Stream A)

-- * Predicates
isPrefixOf      :: Stream A -> Stream A -> Bool
isSuffixOf      :: Stream A -> Stream A -> Bool
isInfixOf       :: Stream A -> Stream A -> Bool

-- * Searching lists
-- ** Searching by equality
elem            :: A -> Stream A -> Bool
notElem         :: A -> Stream A -> Bool
lookup          :: A -> Stream (A, B) -> Maybe B

-- ** Searching with A predicate
find            :: (A -> Bool) -> Stream A -> Maybe A
filter          :: (A -> Bool) -> Stream A -> Stream A
partition       :: (A -> Bool) -> Stream A -> ([A], [A])

-- * Indexing lists
(!!)            :: Stream A -> Int -> A
elemIndex       :: A -> Stream A -> Maybe Int
elemIndices     :: A -> Stream A -> Stream Int
findIndex       :: (A -> Bool) -> Stream A -> Maybe Int
findIndices     :: (A -> Bool) -> Stream A -> Stream Int

-- * Zipping and unzipping lists
zip             :: Stream A -> Stream B -> Stream (A, B)
zip3            :: Stream A -> Stream B -> Stream C -> Stream (A, B, C)
zip4            :: Stream A -> Stream B -> Stream C -> Stream D -> Stream (A, B, C, D)
zip5            :: Stream A -> Stream B -> Stream C -> Stream D -> Stream E -> Stream (A, B, C, D, E)
zip6            :: Stream A -> Stream B -> Stream C -> Stream D -> Stream E -> Stream F -> Stream (A, B, C, D, E, F)
zip7            :: Stream A -> Stream B -> Stream C -> Stream D -> Stream E -> Stream F -> Stream G -> Stream (A, B, C, D, E, F, G)
zipWith         :: (A -> B -> C) -> Stream A -> Stream B -> Stream C
zipWith3        :: (A -> B -> C -> D) -> Stream A -> Stream B -> Stream C -> Stream D
zipWith4        :: (A -> B -> C -> D -> E) -> Stream A -> Stream B -> Stream C -> Stream D -> Stream E
zipWith5        :: (A -> B -> C -> D -> E -> F) -> Stream A -> Stream B -> Stream C -> Stream D -> Stream E -> Stream F
zipWith6        :: (A -> B -> C -> D -> E -> F -> G) -> Stream A -> Stream B -> Stream C -> Stream D -> Stream E -> Stream F -> Stream G
zipWith7        :: (A -> B -> C -> D -> E -> F -> G -> H) -> Stream A -> Stream B -> Stream C -> Stream D -> Stream E -> Stream F -> Stream G -> Stream H
unzip           :: Stream (A, B) -> ([A], [B])
unzip3          :: Stream (A, B, C) -> (Stream A, Stream B, Stream C)
unzip4          :: Stream (A, B, C, D) -> (Stream A, Stream B, Stream C, Stream D)
unzip5          :: Stream (A, B, C, D, E) -> (Stream A, Stream B, Stream C, Stream D, Stream E)
unzip6          :: Stream (A, B, C, D, E, F) -> (Stream A, Stream B, Stream C, Stream D, Stream E, Stream F)
unzip7          :: Stream (A, B, C, D, E, F, G) -> (Stream A, Stream B, Stream C, Stream D, Stream E, Stream F, Stream G)

-- * Special lists
-- ** Functions on strings
lines           :: Stream Char -> Stream String
words           :: Stream Char -> Stream String
unlines         :: Stream String -> Stream Char
unwords         :: Stream String -> Stream Char

-- ** \"Set\" operations
nub             :: Stream A -> Stream A
delete          :: A -> Stream A -> Stream A
(\\)            :: Stream A -> Stream A -> Stream A
union           :: Stream A -> Stream A -> Stream A
intersect       :: Stream A -> Stream A -> Stream A

-- ** Ordered lists
sort            :: Stream OrdA -> Stream OrdA
insert          :: OrdA -> Stream OrdA -> Stream OrdA

-- * Generalized functions
-- ** The \"By\" operations
-- *** User-supplied equality (replacing an Eq context)
nubBy           :: (A -> A -> Bool) -> Stream A -> Stream A
deleteBy        :: (A -> A -> Bool) -> A -> Stream A -> Stream A
deleteFirstsBy  :: (A -> A -> Bool) -> Stream A -> Stream A -> Stream A
unionBy         :: (A -> A -> Bool) -> Stream A -> Stream A -> Stream A
intersectBy     :: (A -> A -> Bool) -> Stream A -> Stream A -> Stream A
groupBy         :: (A -> A -> Bool) -> Stream A -> Stream (Stream A)

-- *** User-supplied comparison (replacing an Ord context)
sortBy          :: (A -> A -> Ordering) -> Stream A -> Stream A
insertBy        :: (A -> A -> Ordering) -> A -> Stream A -> Stream A
maximumBy       :: (A -> A -> Ordering) -> Stream A -> A
minimumBy       :: (A -> A -> Ordering) -> Stream A -> A

-- * The \"generic\" operations
genericLength           :: Stream A -> I
genericTake             :: I -> Stream A -> Stream A
genericDrop             :: I -> Stream A -> Stream A
genericSplitAt          :: I -> Stream A -> ([A], [A])
genericIndex            :: Stream A -> I -> A
genericReplicate        :: I -> A -> Stream A


s :: [a] -> Stream a
s = Stream.stream

u :: Stream a -> [a]
u = Stream.unstream

ss :: [[a]] -> Stream (Stream a)
ss = s . Spec.map s

uu :: Stream (Stream a) -> [[a]]
uu = Spec.map u . u

-- * Basic interface
cons            = \x  xs -> s $ (:)       x (u xs)
snoc            = \xs x  -> s $ (u xs) Spec.++ [x]
(++)            = \xs ys -> s $ (Spec.++)   (u xs) (u ys)
head            = \xs    ->     Spec.head   (u xs)
last            = \xs    ->     Spec.last   (u xs)
tail            = \xs    -> s $ Spec.tail   (u xs)
init            = \xs    -> s $ Spec.init   (u xs)
null            = \xs    ->     Spec.null   (u xs)
length          = \xs    ->     Spec.length (u xs)

-- * List transformations
map             = \f xs     -> s  $ Spec.map         f (u xs)
reverse         = \  xs     -> s  $ Spec.reverse       (u xs)
intersperse     = \x xs     -> s  $ Spec.intersperse x (u xs)
intercalate     = \  xs xss -> s  $ Spec.intercalate   (u xs) (uu xss)
transpose       = \     xss -> ss $ Spec.transpose            (uu xss)

-- * Reducing lists (folds)
foldl           = \f z xs -> Spec.foldl   f z (u xs)
foldl'          = \f z xs -> Spec.foldl'  f z (u xs)
foldl1          = \f   xs -> Spec.foldl1  f   (u xs)
foldl1'         = \f   xs -> Spec.foldl1' f   (u xs)
foldr           = \f z xs -> Spec.foldr   f z (u xs)
foldr1          = \f   xs -> Spec.foldr1  f   (u xs)

-- ** Special folds
concat          = \ xss -> s $ Spec.concat    (uu xss)
concatMap       = \f xs -> s $ Spec.concatMap (u . f) (u xs)
and             = \  xs ->     Spec.and       (u xs)
or              = \  xs ->     Spec.or        (u xs)
any             = \f xs ->     Spec.any     f (u xs)
all             = \f xs ->     Spec.all     f (u xs)
sum             = \  xs ->     Spec.sum       (u xs)
product         = \  xs ->     Spec.product   (u xs)
maximum         = \  xs ->     Spec.maximum   (u xs)
minimum         = \  xs ->     Spec.minimum   (u xs)

-- * Building lists
-- ** Scans
scanl           = \f z xs -> s $ Spec.scanl  f z (u xs)
scanl1          = \f   xs -> s $ Spec.scanl1 f   (u xs)
scanr           = \f z xs -> s $ Spec.scanr  f z (u xs)
scanr1          = \f   xs -> s $ Spec.scanr1 f   (u xs)

-- ** Accumulating maps
mapAccumL       = \f z xs -> (\(a,b)->(a,s b)) $ Spec.mapAccumL f z (u xs)
mapAccumR       = \f z xs -> (\(a,b)->(a,s b)) $ Spec.mapAccumR f z (u xs)

-- ** Infinite lists
iterate         = \f x -> s $ Spec.iterate   f x
repeat          = \  x -> s $ Spec.repeat      x
replicate       = \n x -> s $ Spec.replicate n x
cycle           = \ xs -> s $ Spec.cycle     (u xs)

-- ** Unfolding
unfoldr         = \f x -> s $ Spec.unfoldr f x

-- * Sublists
-- ** Extracting sublists
take            = \n xs -> s  $ Spec.take      n (u xs)
drop            = \n xs -> s  $ Spec.drop      n (u xs)
splitAt         = \n xs ->      Spec.splitAt   n (u xs)
takeWhile       = \f xs -> s  $ Spec.takeWhile f (u xs)
dropWhile       = \f xs -> s  $ Spec.dropWhile f (u xs)
span            = \f xs ->      Spec.span      f (u xs)
break           = \f xs ->      Spec.break     f (u xs)
group           = \  xs -> ss $ Spec.group       (u xs)
inits           = \  xs -> ss $ Spec.inits       (u xs)
tails           = \  xs -> ss $ Spec.tails       (u xs)

-- * Predicates
isPrefixOf      = \xs ys -> Spec.isPrefixOf (u xs) (u ys)
isSuffixOf      = \xs ys -> Spec.isSuffixOf (u xs) (u ys)
isInfixOf       = \xs ys -> Spec.isInfixOf  (u xs) (u ys)

-- * Searching lists
-- ** Searching by equality
elem            = \x xs -> Spec.elem    x (u xs)
notElem         = \x xs -> Spec.notElem x (u xs)
lookup          = \x xs -> Spec.lookup  x (u xs)

-- ** Searching with a predicate
find            = \f xs ->      Spec.find      f (u xs)
filter          = \f xs -> s  $ Spec.filter    f (u xs)
partition       = \f xs ->      Spec.partition f (u xs)

-- * Indexing lists
(!!)            = \xs n ->     (Spec.!!)          (u xs) n
elemIndex       = \x xs ->     Spec.elemIndex   x (u xs)
elemIndices     = \x xs -> s $ Spec.elemIndices x (u xs)
findIndex       = \f xs ->     Spec.findIndex   f (u xs)
findIndices     = \f xs -> s $ Spec.findIndices f (u xs)

-- * Zipping and unzipping lists
zip             = \a b -> s $ Spec.zip (u a) (u b)
zip3            = \a b c -> s $ Spec.zip3 (u a) (u b) (u c)
zip4            = \a b c d -> s $ Spec.zip4 (u a) (u b) (u c) (u d)
zip5            = \a b c d e -> s $ Spec.zip5 (u a) (u b) (u c) (u d) (u e)
zip6            = \a b c d e f -> s $ Spec.zip6 (u a) (u b) (u c) (u d) (u e) (u f)
zip7            = \a b c d e f g -> s $ Spec.zip7 (u a) (u b) (u c) (u d) (u e) (u f) (u g)
zipWith         = \h a b -> s $ Spec.zipWith h (u a) (u b)
zipWith3        = \h a b c -> s $ Spec.zipWith3 h (u a) (u b) (u c)
zipWith4        = \h a b c d -> s $ Spec.zipWith4 h (u a) (u b) (u c) (u d)
zipWith5        = \h a b c d e -> s $ Spec.zipWith5 h (u a) (u b) (u c) (u d) (u e)
zipWith6        = \h a b c d e f -> s $ Spec.zipWith6 h (u a) (u b) (u c) (u d) (u e) (u f)
zipWith7        = \h a b c d e f g -> s $ Spec.zipWith7 h (u a) (u b) (u c) (u d) (u e) (u f) (u g)
unzip           = \xs -> {-(\(a,b)->(s a,s b)) $-} Spec.unzip (u xs)
unzip3          = \xs -> (\(a,b,c)->(s a,s b,s c)) $ Spec.unzip3 (u xs)
unzip4          = \xs -> (\(a,b,c,d)->(s a,s b,s c,s d)) $ Spec.unzip4 (u xs)
unzip5          = \xs -> (\(a,b,c,d,e)->(s a,s b,s c,s d,s e)) $ Spec.unzip5 (u xs)
unzip6          = \xs -> (\(a,b,c,d,e,f)->(s a,s b,s c,s d,s e,s f)) $ Spec.unzip6 (u xs)
unzip7          = \xs -> (\(a,b,c,d,e,f,g)->(s a,s b,s c,s d,s e,s f,s g)) $ Spec.unzip7 (u xs)

-- * Special lists
-- ** Functions on strings
lines           = \x -> s $ Spec.lines (u x)
words           = \x -> s $ Spec.words (u x)
unlines         = \x -> s $ Spec.unlines (u x)
unwords         = \x -> s $ Spec.unwords (u x)

-- ** \"Set\" operations
nub             = \  xs    -> s $ Spec.nub       (u xs)
delete          = \x xs    -> s $ Spec.delete x  (u xs)
(\\)            = \  xs ys -> s $ (Spec.\\)      (u xs) (u ys)
union           = \  xs ys -> s $ Spec.union     (u xs) (u ys)
intersect       = \  xs ys -> s $ Spec.intersect (u xs) (u ys)

-- ** Ordered lists
sort            = \  xs -> s $ Spec.sort     (u xs)
insert          = \x xs -> s $ Spec.insert x (u xs)

-- * Generalized functions
-- ** The \"By\" operations
-- *** User-supplied equality (replacing an Eq context)
nubBy           = \f   xs    -> s  $ Spec.nubBy          f   (u xs)
deleteBy        = \f x xs    -> s  $ Spec.deleteBy       f x (u xs)
deleteFirstsBy  = \f   xs ys -> s  $ Spec.deleteFirstsBy f   (u xs) (u ys)
unionBy         = \f   xs ys -> s  $ Spec.unionBy        f   (u xs) (u ys)
intersectBy     = \f   xs ys -> s  $ Spec.intersectBy    f   (u xs) (u ys)
groupBy         = \f   xs    -> ss $ Spec.groupBy        f   (u xs)

-- *** User-supplied comparison (replacing an Ord context)
sortBy          = \f   xs -> s $ Spec.sortBy    f   (u xs)
insertBy        = \f x xs -> s $ Spec.insertBy  f x (u xs)
maximumBy       = \f   xs ->     Spec.maximumBy f   (u xs)
minimumBy       = \f   xs ->     Spec.minimumBy f   (u xs)

-- * The \"generic\" operations
genericLength           = \  xs ->      Spec.genericLength      (u xs)
genericTake             = \n xs -> s  $ Spec.genericTake      n (u xs)
genericDrop             = \n xs -> s  $ Spec.genericDrop      n (u xs)
genericSplitAt          = \n xs ->      Spec.genericSplitAt   n (u xs)
genericIndex            = \xs n ->      Spec.genericIndex       (u xs) n
genericReplicate        = \n x  -> s  $ Spec.genericReplicate n x
