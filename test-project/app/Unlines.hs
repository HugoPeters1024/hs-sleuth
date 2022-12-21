{-# LANGUAGE TemplateHaskell #-}
module Unlines where

import HsComprehension.Plugin (dumpThisModule)
import qualified Data.List.Stream as S

--unlines_stream :: [String] -> String
--unlines_stream ls = S.concat (S.map (\l -> l S.++ ['\n']) ls)

unlines :: [String] -> String
unlines ls = concat (map (\l -> l ++ ['\n']) ls)

--dumpThisModule
