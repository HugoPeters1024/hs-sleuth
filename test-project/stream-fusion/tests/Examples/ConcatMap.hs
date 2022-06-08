module M where
import Char

import Data.List.Stream as L

foo :: [Char] -> [Char]
foo xs = (L.concatMap (L.replicate 10000)) ( map toUpper xs)

