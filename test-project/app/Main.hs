module Main where

import Data.Char (toUpper)
import Data.Text (Text)
import qualified Data.Text as T

uppercaseLength :: Text -> Int
uppercaseLength xs = T.length (T.map id xs)

main :: IO ()
main = pure ()
