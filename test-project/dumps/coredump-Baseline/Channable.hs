module Channable where

import Data.Text (Text)
import qualified Data.Text as T

slice :: Int -> Int -> Text -> Text
slice offset len = T.take len . T.drop offset

{-# NOINLINE noInlineTake #-}
noInlineTake :: Int -> Text -> Text
noInlineTake = T.take

