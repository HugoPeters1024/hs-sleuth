module Fusion where

import Prelude hiding (sum, scanl, zip3, zipWith3, map)
import Data.Stream
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

--addThree :: [Int] -> [Int]
--addThree = Data.List.Stream.map (+1) . Data.List.Stream.map(+2)
--
--addThree2 :: [Int] -> [Int]
--addThree2 xs = Data.Stream.unstream (Data.Stream.map (+3) (Data.Stream.stream xs))

addMany :: [Int] -> [Int]
addMany 
  = Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)
  . Data.List.Stream.map (+1)

--totalPrefixSum :: [Int] -> Int
--totalPrefixSum = sum . scanl (+) 0


