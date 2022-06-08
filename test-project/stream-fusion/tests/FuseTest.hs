module FuseTest where

import Data.List.Stream as L

foo :: Int -> Int
foo n = L.sum (L.replicate n 1)
