module FuseTest where

import Data.List as L

foo :: Int -> Int
foo n = L.sum (L.replicate n 1)
