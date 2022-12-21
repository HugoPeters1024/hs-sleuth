module Streaming where

import qualified Data.List.Stream as S

halves :: [Int] -> [Int]
halves xs = S.map (`div` 2) (S.filter even xs)

--div2_s :: [Int] -> [Int]
--div2_s xs = S.map (`div` 2) $ S.filter even xs
--
--doubleSum :: [Int] -> Int
--doubleSum = sum . map (*2)


