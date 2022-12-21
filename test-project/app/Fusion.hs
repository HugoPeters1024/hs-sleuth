module Fusion where

import Prelude hiding (sum, scanl, zip3, zipWith3, map)
import Data.Stream

import HsComprehension.Plugin (dumpThisModule)

import qualied Data.List.Stream as S

dumpThisModule

halves :: [Int] -> [Int]
halves = map (`div` 2) . filter even

halves_stream :: [Int] -> [Int]
halves_stream = S.map (`div` 2) . S.filter even


