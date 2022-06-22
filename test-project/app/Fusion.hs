module Fusion where

import Prelude hiding (sum, scanl, zip3, zipWith3, map)
import Data.List.Stream

--data User = User
--    { first_name :: String
--    , last_name :: String
--    , age :: Int
--    }
--    deriving (Show)
--
--ages = [24, 69, 420]
--firstNames = ["Hugo", "Brian", "Micheal"]
--lastNames = ["Luck", "Stevens", "Broer"]
--
--users :: [User]
--users = zipWith3 User firstNames lastNames ages

addThree :: [Int] -> [Int]
addThree = map (+1) . map(+2)

--totalPrefixSum :: [Int] -> Int
--totalPrefixSum = sum . scanl (+) 0


