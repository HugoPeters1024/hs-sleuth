module Unlines where



unlines :: [String] -> String
unlines ls = concat (map (\l -> l ++ ['\n']) ls)
