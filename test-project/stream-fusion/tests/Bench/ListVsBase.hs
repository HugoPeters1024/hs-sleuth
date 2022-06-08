{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
--
-- Benchmark tool.
-- Compare a function against equivalent code from other libraries for
-- space and time.
--
import Bench.Utils

import qualified Data.List        as L -- theirs
import qualified Data.List.Stream as S -- ours 

import Data.Char
import Data.Word
import Data.Int

import System.IO
import Control.Monad
import Text.Printf

main :: IO ()
main = do
    -- initialise
    let input = Input (take 1000 string) string string2 (splitEvery 1000 string2)
    force input

    putStrLn "Benchmarking Data.List.Stream <=> Data.List"
    putStrLn "===========================================\n"

    printf "# Size of test data: %dk\n" ((floor $ (fromIntegral (length string)) / 1024) :: Int)
    printf "#List\t List.Stream\n"


    run 5 input tests

------------------------------------------------------------------------

tests =
 [
    -- * Basic interface

    ("++",  -- should be identical
        [F ( app  (uncurry ((L.++) :: S -> S -> S) )    )
        ,F ( app  (uncurry ((S.++) :: S -> S -> S) )    ) ]
    )

    , ("head",
        [F ( app  (L.head :: S -> Char) )
        ,F ( app  (S.head :: S -> Char) )]
    )

    , ("last",
        [F (     app (L.last :: S -> Char))
        ,F (     app (S.last :: S -> Char))
    ])

    , ("init",
        [F (     app (L.init :: S -> S))
        ,F (     app (S.init :: S -> S))
    ])

    , ("null",
        [F (     app (L.null :: S -> Bool))
        ,F (     app (S.null :: S -> Bool))
    ])

    , ("length",
        [F (      app  (L.length :: S -> Int))
        ,F (      app  (S.length :: S -> Int))
    ])

    -- * List transformations
    , ("map",
        [F (      app ( L.map toUpper :: S -> S ))
        ,F (      app ( S.map toUpper :: S -> S ))
    ])

    , ("reverse",
        [F (     app (L.reverse :: S -> S ))
        ,F (     app (S.reverse :: S -> S ))
    ])

    , ("intersperse",
        [F ( app  (L.intersperse 'x' :: S -> S)     )
        ,F ( app  (S.intersperse 'x' :: S -> S)     ) ]
     )

    , ("intercalate",
        [F ( app2  (L.intercalate :: S -> [S] -> S)     )
        ,F ( app2  (S.intercalate :: S -> [S] -> S)     ) ]
     )

    -- transpose is too slow.

    -- * Reducing lists (folds)

    , ("foldl'",
        [F (     app  ( L.foldl' (\a _ -> a+1) 0 :: S -> Int  ) )
        ,F (     app  ( S.foldl' (\a _ -> a+1) 0  :: S -> Int ) )
    ])

{-
    , ("foldr",
        [F (     app  ( L.foldr (\_ a -> a+1) 0 :: S -> Int  ) )
        ,F (     app  ( S.foldr (\_ a -> a+1) 0 :: S -> Int ) )
    ])
-}

    -- ** Special folds

    , ("concat",
        [F ( app   ((\ss -> L.concat (ss++ss++ss)) :: [S] -> S)               )
        ,F ( app   ((\ss -> S.concat (ss++ss++ss)) :: [S] -> S)               ) ]
     )

    , ("concatMap",
        [F ( app   (L.concatMap (\c -> L.replicate 10 c) :: S -> S))
        ,F ( app   (S.concatMap (\c -> S.replicate 10 c) :: S -> S)) ]
     )

    , ("any",
        [F (     app ( L.any (=='x') :: S -> Bool       ))
        ,F (     app ( S.any (=='x') :: S -> Bool       ))
    ])
    , ("all",
        [F (     app ( L.all (=='x') :: S -> Bool       ))
        ,F (     app ( S.all (=='x') :: S -> Bool       ))
    ])
    , ("maximum",
        [{-F (     app (L.maximum :: S -> Char) )
        ,-}F (     app (S.maximum :: S -> Char) )
    ])
    , ("minimum",
        [{-F (     app (L.minimum :: S -> Char) )
        ,-}F (     app (S.minimum :: S -> Char) )
    ])

    -- * Building lists
    -- ** Scans

    -- * Sublists
    -- ** Extracting sublists
    , ("take",
        [F (     app ( L.take 100000 :: S -> S) )
        ,F (     app ( S.take 100000 :: S -> S) )
    ])
    , ("drop",
        [F (     app ( L.drop 100000 :: S -> S) )
        ,F (     app ( S.drop 100000 :: S -> S) )
    ])

    , ("takeWhile",
        [F (     app ( L.takeWhile (/='z') :: S -> S  ))
        ,F (    app  ( S.takeWhile (=='z')  :: S -> S ))
    ])
    , ("dropWhile",
        [F (     app ( L.dropWhile (/='z')  :: S -> S ) )
        ,F (    app  ( S.dropWhile (/='z')  :: S -> S ) )
    ])

    -- * Searching lists
    -- ** Searching by equality
    , ("elem",
        [F (     app (L.elem ('Z') :: S -> Bool) )
        ,F (     app (S.elem ('Z') :: S -> Bool) )
    ])
    , ("notElem",
        [F (     app (L.notElem ('Z') :: S -> Bool) )
        ,F (     app (S.notElem ('Z') :: S -> Bool) )
    ])

    -- ** Searching with a predicate

    , ("find",
        [F (     app (L.find (=='Z') :: S -> Maybe Char) )
        ,F (     app (S.find (=='Z') :: S -> Maybe Char) )
    ])

    , ("filter",
        [F (      app ( L.filter isSpace :: S -> S ))
        ,F (      app ( S.filter isSpace :: S -> S ))
    ])

    -- * Indexing lists

    , ("index",
        [F (      app ((\x -> x L.!! 300000) ::  S -> Char  ))
        ,F (      app ((\x -> x S.!! 300000) ::  S -> Char  ))
    ])

    , ("elemIndex",
        [F (    app (L.elemIndex ('Z') :: S -> Maybe Int ) )
        ,F (    app (S.elemIndex ('Z') :: S -> Maybe Int ) )
    ])

    , ("elemIndices",
        [F (    app (L.elemIndices ('Z') :: S -> [Int] ) )
        ,F (    app (S.elemIndices ('Z') :: S -> [Int] ) )
    ])

    , ("findIndex",
        [F (    app (L.findIndex (=='Z') :: S -> Maybe Int ) )
        ,F (    app (S.findIndex (=='Z') :: S -> Maybe Int ) )
    ])

    , ("findIndices",
        [F (    app (L.findIndices (=='Z') :: S -> [Int] ) )
        ,F (    app (S.findIndices (=='Z') :: S -> [Int] ) )
    ])

    -- * Zipping and unzipping lists

    , ("zip",
        [F (     app (uncurry (L.zip) :: (S,S) -> [(Char, Char)]     ))
        ,F (     app (uncurry (S.zip) :: (S,S) -> [(Char, Char)]     ))
    ])

    , ("zipWith",
        [F (     app (uncurry (L.zipWith (,)) :: (S,S) -> [(Char, Char)]     ))
        ,F (     app (uncurry (S.zipWith (,)) :: (S,S) -> [(Char, Char)]     ))
    ])

    , ("replicate",
        [F (    const $ L.replicate 2000000 'x')
        ,F (    const $ S.replicate 2000000 'x')
    ])

 ]


{-


    , ("span",
        [F ({-# SCC "span"          #-}     app $ B.span (/=122))
        ,F ({-# SCC "lazy span"     #-}     app $ L.span (/=122))
    ])
    , ("break",
        [F ({-# SCC "break"         #-}     app $ B.break (==122))
        ,F ({-# SCC "lazy break"    #-}     app $ L.break (==122))
    ])
    , ("split",
        [F ({-# SCC "split"         #-}     app $ B.split 0x0a)
        ,F ({-# SCC "lazy split"    #-}     app $ L.split 0x0a)
    ])
--  , ("breakByte",
--      [F ({-# SCC "breakChar"     #-}     app $ B.breakByte 122)
--      ,F ({-# SCC "lazy breakChar" #-}    app $ L.breakByte 122)
--  ])
--  , ("spanByte",
--      [F ({-# SCC "spanChar"      #-}     app $ B.spanByte 122)
--      ,F ({-# SCC "lazy spanChar" #-}     app $ L.spanByte 122)
--  ])

    , ("cons",
        [F ({-# SCC "cons"          #-}     app $ B.cons 120)
        ,F ({-# SCC "lazy cons"     #-}     app $ L.cons 120)
    ])
    , ("snoc",
        [F ({-# SCC "snoc"          #-}     app $ flip B.snoc 120)
        ,F ({-# SCC "lazy snoc"     #-}     app $ flip L.snoc 120)
    ])
    , ("empty",
        [F ({-# SCC "empty"         #-}     const B.empty)
        ,F ({-# SCC "lazy empty"    #-}     const L.empty)
    ])
    , ("head",
        [F ({-# SCC "head"          #-}     app B.head)
        ,F ({-# SCC "lazy head"     #-}     app L.head)
    ])
    , ("tail",
        [F ({-# SCC "tail"          #-}     app B.tail)
        ,F ({-# SCC "lazy tail"     #-}     app L.tail)
    ])

    , ("count",
        [F ({-# SCC "count"         #-}     app $ B.count 10)
        ,F ({-# SCC "lazy count"    #-}     app $ L.count 10)
    ])

    , ("isPrefixOf",
        [F ({-# SCC "isPrefixOf" #-}        app $ B.isPrefixOf
                (C.pack "The Project Gutenberg eBook"))
        ,F ({-# SCC "lazy isPrefixOf" #-}   app $ L.isPrefixOf
                (L.pack [84,104,101,32,80,114,111,106,101
                           ,99,116,32,71,117,116,101,110,98
                           ,101,114,103,32,101,66,111,111,107]))
    ])
    , ("join",
        [F ({-# SCC "join"          #-}     app $ B.join (B.pack [1,2,3]))
        ,F ({-# SCC "lazy join"     #-}     app $ L.join (L.pack [1,2,3]))
    ])
--  , ("joinWithByte",
--      [F ({-# SCC "joinWithByte"  #-}     app $ uncurry (B.joinWithByte 32))
--      ,F ({-# SCC "lazy joinWithByte" #-} app $ uncurry (L.joinWithByte 32))
--  ])

    , ("elem",
        [F ({-# SCC "elem"          #-}     app $ B.elem 122)
        ,F ({-# SCC "lazy elem"     #-}     app $ L.elem 122)
    ])
    , ("notElem",
        [F ({-# SCC "notElem"       #-}     app $ B.notElem 122)
        ,F ({-# SCC "lazy notElem"  #-}     app $ L.notElem 122)
    ])
    , ("elemIndex",
        [F ({-# SCC "elemIndex"     #-}     app $ B.elemIndex 122)
        ,F ({-# SCC "lazy elemIndex" #-}    app $ L.elemIndex 122)
    ])
    , ("findIndices",
        [F ({-# SCC "findIndicies"  #-}     app $ B.findIndices (==122))
        ,F ({-# SCC "lazy findIndices" #-}  app $ L.findIndices (==122))
    ])
    , ("elemIndices",
        [F ({-# SCC "elemIndicies"  #-}     app $ B.elemIndices 122)
        ,F ({-# SCC "lazy elemIndices" #-}  app $ L.elemIndices 122)
    ])
    , ("splitAt",
        [F ({-# SCC "splitAt"       #-}     app $ B.splitAt 10000)
        ,F ({-# SCC "lazy splitAt"  #-}     app $ L.splitAt 10000)
    ])
    , ("splitWith",
        [F ({-# SCC "splitWith"     #-}     app $ B.splitWith (==122))
        ,F ({-# SCC "lazy splitWith" #-}    app $ L.splitWith (==122))
    ])

    , ("group",
        [F ({-# SCC "group"         #-}     app B.group)
        ,F ({-# SCC "lazy group"    #-}     app L.group)
    ])
    , ("groupBy",
        [F ({-# SCC "groupBy"       #-}     app $ B.groupBy (==))
        ,F ({-# SCC "lazy groupBy"  #-}     app $ L.groupBy (==))
    ])
    , ("inits",
        [F ({-# SCC "inits"         #-}     app B.inits)
    ])
    , ("tails",
        [F ({-# SCC "tails"         #-}     app B.tails)
    ])
--  , ("transpose",[F ({-# SCC "transpose" #-}B.transpose [fps,fps'])])

------------------------------------------------------------------------
--
-- Char8 or ByteString only

    , ("intersperse",
        [F ({-# SCC "intersperse"   #-}     app $ B.intersperse 120 )
    ])
    , ("sort",
        [F ({-# SCC "sort"          #-}     app B.sort)
    ])
--  , ("lineIndices",
--      [F ({-# SCC "lineIndicies"  #-}     app C.lineIndices)
--  ])
    , ("elemIndexEnd",
        [F ({-# SCC "elemIndexEnd"  #-}     app $ B.elemIndexEnd 122)
    ])
--  , ("breakSpace",
--      [F ({-# SCC "breakSpace"    #-}     app C.breakSpace)
--  ])
--  , ("dropSpace",
--      [F ({-# SCC "dropSpace"     #-}     app C.dropSpace)
--  ])
--  , ("dropSpaceEnd",
--      [F ({-# SCC "dropSpaceEnd"  #-}     app C.dropSpaceEnd)
--  ])

--  , ("zip",[F ({-# SCC "zip" #-} B.zip fps fps)])

    , ("isSubstringOf",
        [F ({-# SCC "isSubstringOf" #-}     app $ B.isSubstringOf (C.pack "email news"))
    ])
    , ("isSuffixOf",
        [F ({-# SCC "isSuffixOf"    #-}     app $ B.isSuffixOf (C.pack "new eBooks"))
    ])
    , ("spanEnd",
        [F ({-# SCC "spanEnd"       #-}     app $ B.spanEnd (/=122))
    ])
    , ("lines",
        [F ({-# SCC "lines"         #-}     app C.lines)
    ])
    , ("unlines",
        [F ({-# SCC "unlines"       #-}     app C.unlines)
    ])
    , ("words",
        [F ({-# SCC "words"         #-}     app C.words)
    ])
    , ("unwords",
        [F ({-# SCC "unwords"       #-}     app C.unwords)
    ])

 ]
-}

------------------------------------------------------------------------

data Input = Input String String String [String]

instance Forceable Input where
  force (Input s x y xs) = force s >> force x >> force y >> force xs

class (Eq a, Ord a) => Ap a where app :: (a -> b) -> Input -> b

instance Ap String            where app f (Input _ x _ _)  = f x
instance Ap [String]          where app f (Input _ _ _ xs) = f xs
instance Ap (String,String)   where app f (Input _ x y _)  = f (x, y)
instance Ap (String,[String]) where app f (Input s _ _ xs) = f (s, xs)

app2 :: Ap (a, b) => (a -> b -> c) -> Input -> c
app2 = app . uncurry

splitEvery :: Int -> [a] -> [[a]]
splitEvery n = L.unfoldr split
  where split [] = Nothing
        split s  = Just (Prelude.splitAt n s)
