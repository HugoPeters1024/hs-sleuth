
--
-- Must have rules off, otherwise the fusion rules will replace the rhs
-- with the lhs, and we only end up testing lhs == lhs
--

--
-- Test the new list implementation against Data.List.
--

import Properties.Utils
import System.IO

import qualified Properties.Monomorphic.List as Test        -- our implementation
import qualified Properties.Monomorphic.Base as Spec        -- current Data.List

--
-- Data.List.Stream <=> Data.List
--

------------------------------------------------------------------------
-- * Basic interface

prop_append     = (Test.++)     `eq2`           (Spec.++)
prop_head       = Test.head     `eqnotnull1`    Spec.head
prop_last       = Test.last     `eqnotnull1`    Spec.last
prop_tail       = Test.tail     `eqnotnull1`    Spec.tail
prop_init       = Test.init     `eqnotnull1`    Spec.init
prop_null       = Test.null     `eq1`           Spec.null
prop_length     = Test.length   `eq1`           Spec.length

------------------------------------------------------------------------
-- * List transformations

prop_map                = Test.map              `eq2`   Spec.map
prop_reverse            = Test.reverse          `eq1`   Spec.reverse
prop_intersperse        = Test.intersperse      `eq2`   Spec.intersperse
prop_intercalate        = Test.intercalate      `eq2`   Spec.intercalate
prop_transpose          = Test.transpose        `eq1`   Spec.transpose

------------------------------------------------------------------------
-- * Reducing lists (folds)

prop_foldl      = Test.foldl            `eq3`   Spec.foldl
prop_foldl'     = Test.foldl'           `eq3`   Spec.foldl'
prop_foldl1     = Test.foldl1   `eqnotnull2`    Spec.foldl1
prop_foldl1'    = Test.foldl1'  `eqnotnull2`    Spec.foldl1'
prop_foldr      = Test.foldr            `eq3`   Spec.foldr
prop_foldr1     = Test.foldr1   `eqnotnull2`    Spec.foldr1

------------------------------------------------------------------------
-- ** Special folds

prop_concat     = Test.concat           `eq1`   Spec.concat
prop_concatMap  = Test.concatMap        `eq2`   Spec.concatMap
prop_and        = Test.and              `eq1`   Spec.and
prop_or         = Test.or               `eq1`   Spec.or
prop_any        = Test.any              `eq2`   Spec.any
prop_all        = Test.all              `eq2`   Spec.all
prop_sum        = Test.sum              `eq1`   Spec.sum
prop_product    = Test.product          `eq1`   Spec.product
prop_maximum    = Test.maximum  `eqnotnull1`    Spec.maximum
prop_minimum    = Test.minimum  `eqnotnull1`    Spec.minimum

------------------------------------------------------------------------
-- * Building lists
-- ** Scans

prop_scanl      = Test.scanl            `eq3`   Spec.scanl
prop_scanl1     = Test.scanl1           `eq2`   Spec.scanl1
prop_scanr      = Test.scanr            `eq3`   Spec.scanr
prop_scanr1     = Test.scanr1           `eq2`   Spec.scanr1

------------------------------------------------------------------------
-- ** Accumulating maps

prop_mapAccumL  = Test.mapAccumL        `eq3`   Spec.mapAccumL
prop_mapAccumR  = Test.mapAccumR        `eq3`   Spec.mapAccumR

------------------------------------------------------------------------
-- ** Infinite lists

prop_iterate    = Test.iterate          `eqfinite2`     Spec.iterate
prop_repeat     = Test.repeat           `eqfinite1`     Spec.repeat
prop_replicate  = Test.replicate        `eq2`           Spec.replicate
prop_cycle      = \x -> not (null x) ==>
                  (Test.cycle           `eqfinite1`     Spec.cycle) x

------------------------------------------------------------------------
-- ** Unfolding

prop_unfoldr    = Test.unfoldr          `eqfinite2`     Spec.unfoldr

------------------------------------------------------------------------
-- * Sublists
-- ** Extracting sublists

prop_take       = Test.take             `eq2`   Spec.take
prop_drop       = Test.drop             `eq2`   Spec.drop
prop_splitAt    = Test.splitAt          `eq2`   Spec.splitAt
prop_takeWhile  = Test.takeWhile        `eq2`   Spec.takeWhile
prop_dropWhile  = Test.dropWhile        `eq2`   Spec.dropWhile
prop_span       = Test.span             `eq2`   Spec.span
prop_break      = Test.break            `eq2`   Spec.break
prop_group      = Test.group            `eq1`   Spec.group
prop_inits      = Test.inits            `eq1`   Spec.inits
prop_tails      = Test.tails            `eq1`   Spec.tails

------------------------------------------------------------------------
-- * Predicates

prop_isPrefixOf  = Test.isPrefixOf       `eq2`   Spec.isPrefixOf
prop_isSuffixOf  = Test.isSuffixOf       `eq2`   Spec.isSuffixOf
prop_isInfixOf   = Test.isInfixOf        `eq2`   Spec.isInfixOf

------------------------------------------------------------------------
-- * Searching lists
-- ** Searching by equality

prop_elem       = Test.elem             `eq2`   Spec.elem
prop_notElem    = Test.notElem          `eq2`   Spec.notElem
prop_lookup     = Test.lookup           `eq2`   Spec.lookup

------------------------------------------------------------------------
-- ** Searching with a predicate

prop_find       = Test.find             `eq2`   Spec.find
prop_filter     = Test.filter           `eq2`   Spec.filter
prop_partition  = Test.partition        `eq2`   Spec.partition

------------------------------------------------------------------------
-- * Indexing lists

prop_index              = \xs n -> n >= 0 && n < length xs ==>
                          ((Test.!!)            `eq2`   (Spec.!!)) xs n
prop_elemIndex          = Test.elemIndex        `eq2`   Spec.elemIndex
prop_elemIndices        = Test.elemIndices      `eq2`   Spec.elemIndices
prop_findIndex          = Test.findIndex        `eq2`   Spec.findIndex
prop_findIndices        = Test.findIndices      `eq2`   Spec.findIndices

------------------------------------------------------------------------
-- * Zipping and unzipping lists

prop_zip        = Test.zip              `eq2`   Spec.zip
prop_zip3       = Test.zip3             `eq3`   Spec.zip3
prop_zip4       = Test.zip4             `eq4`   Spec.zip4
prop_zip5       = Test.zip5             `eq5`   Spec.zip5
prop_zip6       = Test.zip6             `eq6`   Spec.zip6
prop_zip7       = Test.zip7             `eq7`   Spec.zip7
prop_zipWith    = Test.zipWith          `eq3`   Spec.zipWith
prop_zipWith3   = Test.zipWith3         `eq4`   Spec.zipWith3
prop_zipWith4   = Test.zipWith4         `eq5`   Spec.zipWith4
prop_zipWith5   = Test.zipWith5         `eq6`   Spec.zipWith5
prop_zipWith6   = Test.zipWith6         `eq7`   Spec.zipWith6
prop_zipWith7   = Test.zipWith7         `eq8`   Spec.zipWith7

------------------------------------------------------------------------

prop_unzip      = Test.unzip            `eq1`   Spec.unzip
prop_unzip3     = Test.unzip3           `eq1`   Spec.unzip3
prop_unzip4     = Test.unzip4           `eq1`   Spec.unzip4
prop_unzip5     = Test.unzip5           `eq1`   Spec.unzip5
prop_unzip6     = Test.unzip6           `eq1`   Spec.unzip6
prop_unzip7     = Test.unzip7           `eq1`   Spec.unzip7

------------------------------------------------------------------------
-- * Special lists
-- ** Functions on strings

prop_lines      = Test.lines            `eq1`   Spec.lines
prop_words      = Test.words            `eq1`   Spec.words
prop_unlines    = Test.unlines          `eq1`   Spec.unlines
prop_unwords    = Test.unwords          `eq1`   Spec.unwords

------------------------------------------------------------------------
-- ** \"Set\" operations

prop_nub        = Test.nub              `eq1`   Spec.nub
prop_delete     = Test.delete           `eq2`   Spec.delete
prop_difference = (Test.\\)             `eq2`   (Spec.\\)
prop_union      = Test.union            `eq2`   Spec.union
prop_intersect  = Test.intersect        `eq2`   Spec.intersect

------------------------------------------------------------------------
-- ** Ordered lists 

prop_sort       = Test.sort             `eq1`   Spec.sort
prop_insert     = Test.insert           `eq2`   Spec.insert

------------------------------------------------------------------------
-- * Generalized functions
-- ** The \"By\" operations
-- *** User-supplied equality (replacing an Eq context)

prop_nubBy              = Test.nubBy            `eq2`   Spec.nubBy
prop_deleteBy           = Test.deleteBy         `eq3`   Spec.deleteBy
prop_deleteFirstsBy     = Test.deleteFirstsBy   `eq3`   Spec.deleteFirstsBy
prop_unionBy            = Test.unionBy          `eq3`   Spec.unionBy
prop_intersectBy        = Test.intersectBy      `eq3`   Spec.intersectBy
prop_groupBy            = Test.groupBy          `eq2`   Spec.groupBy

------------------------------------------------------------------------
-- *** User-supplied comparison (replacing an Ord context)

prop_sortBy             = Test.sortBy           `eq2`           Spec.sortBy
prop_insertBy           = Test.insertBy         `eq3`           Spec.insertBy
prop_maximumBy          = Test.maximumBy        `eqnotnull2`    Spec.maximumBy
prop_minimumBy          = Test.minimumBy        `eqnotnull2`    Spec.minimumBy

------------------------------------------------------------------------
-- * The \"generic\" operations

prop_genericLength      = Test.genericLength    `eq1`   Spec.genericLength
prop_genericTake        = \i -> i >= I 0 ==>
                          (Test.genericTake     `eq2`   Spec.genericTake) i
prop_genericDrop        = \i -> i >= I 0 ==>
                          (Test.genericDrop     `eq2`   Spec.genericDrop) i
prop_genericSplitAt     = \i -> i >= I 0 ==>
                          (Test.genericSplitAt  `eq2`   Spec.genericSplitAt) i
prop_genericIndex       = \xs i -> i >= I 0 && i < Spec.genericLength xs ==>
                          (Test.genericIndex    `eq2`   Spec.genericIndex) xs i
prop_genericReplicate   = \i -> i >= I 0 ==>
                          (Test.genericReplicate        `eq2`   Spec.genericReplicate) i

------------------------------------------------------------------------

main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Testing: Data.List.Stream <=> Data.List"
  putStrLn "=======================================\n"

  runTests "Basic interface" opts
    [run prop_append
    ,run prop_head
    ,run prop_last
    ,run prop_tail
    ,run prop_init
    ,run prop_null
    ,run prop_length
    ]

  runTests "List transformations" opts
    [run prop_map
    ,run prop_reverse
    ,run prop_intersperse
    ,run prop_intercalate
    ,run prop_transpose
    ]

  runTests "Reducing lists (folds)" opts
    [run prop_foldl
    ,run prop_foldl'
    ,run prop_foldl1
    ,run prop_foldl1'
    ,run prop_foldr
    ,run prop_foldr1
    ]

  runTests "Special folds" opts
    [run prop_concat
    ,run prop_concatMap
    ,run prop_and
    ,run prop_or
    ,run prop_any
    ,run prop_all
    ,run prop_sum
    ,run prop_product
    ,run prop_maximum
    ,run prop_minimum
    ]

  runTests "Scans" opts
    [run prop_scanl
    ,run prop_scanl1
    ,run prop_scanr
    ,run prop_scanr1
    ]

  runTests "Accumulating maps" opts
    [run prop_mapAccumL
    ,run prop_mapAccumR
    ]

  runTests "Infinite lists" opts
    [run prop_iterate
    ,run prop_repeat
    ,run prop_replicate
    ,run prop_cycle
    ]

  runTests "Unfolding" opts
    [run prop_unfoldr
    ]

  runTests "Extracting sublists" opts
    [run prop_take
    ,run prop_drop
    ,run prop_splitAt
    ,run prop_takeWhile
    ,run prop_dropWhile
    ,run prop_span
    ,run prop_break
    ,run prop_group
    ,run prop_inits
    ,run prop_tails
    ]

  runTests "Predicates" opts
    [run prop_isPrefixOf
    ,run prop_isSuffixOf
    ,run prop_isInfixOf
    ]

  runTests "Searching by equality" opts
    [run prop_elem
    ,run prop_notElem
    ,run prop_lookup
    ]

  runTests "Searching by a predicate" opts
    [run prop_find
    ,run prop_filter
    ,run prop_partition
    ]

  runTests "Indexing lists" opts
    [run prop_index
    ,run prop_elemIndex
    ,run prop_elemIndices
    ,run prop_findIndex
    ,run prop_findIndices
    ]

  runTests "Zipping" opts
    [run prop_zip
    ,run prop_zip3
    ,run prop_zip4
    ,run prop_zip5
    ,run prop_zip6
    ,run prop_zip7
    ,run prop_zipWith
    ,run prop_zipWith3
    ,run prop_zipWith4
    ,run prop_zipWith5
    ,run prop_zipWith6
    ,run prop_zipWith7
    ]

  runTests "Unzipping" opts
    [run prop_unzip
    ,run prop_unzip3
    ,run prop_unzip4
    ,run prop_unzip5
    ,run prop_unzip6
    ,run prop_unzip7
    ]

  runTests "Functions on strings" opts
    [run prop_lines
    ,run prop_words
    ,run prop_unlines
    ,run prop_unwords
    ]

  runTests "\"Set\" operations" opts
    [run prop_nub
    ,run prop_delete
    ,run prop_difference
    ,run prop_union
    ,run prop_intersect
    ]

  runTests "Ordered lists" opts
    [run prop_sort
    ,run prop_insert
    ]

  runTests "Eq style \"By\" operations" opts
    [run prop_nubBy
    ,run prop_deleteBy
    ,run prop_deleteFirstsBy
    ,run prop_unionBy
    ,run prop_intersectBy
    ,run prop_groupBy
    ]

  runTests "Ord style \"By\" operations" opts
    [run prop_sortBy        -- note issue here.
    ,run prop_insertBy
    ,run prop_maximumBy
    ,run prop_minimumBy
    ]

  runTests "The \"generic\" operations" opts
    [run prop_genericLength
    ,run prop_genericTake
    ,run prop_genericDrop
    ,run prop_genericSplitAt
    ,run prop_genericIndex
    ,run prop_genericReplicate
    ]
