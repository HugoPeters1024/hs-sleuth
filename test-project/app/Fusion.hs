module Fusion where

import Prelude hiding (sum, scanl)
import Data.List.Stream

totalPrefixSum :: [Int] -> Int
totalPrefixSum = sum . scanl (+) 0


