module Text where

import Data.Text (Text)
import qualified Data.Text as T

{-# INLINE slice #-}
slice :: Int -> Int -> Text -> Text
slice offset len = T.take len . T.drop offset
