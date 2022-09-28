module Text where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

{-# INLINE slice #-}
slice :: Int -> Int -> Text -> Text
slice offset len = T.take len . T.drop offset

-- from: https://arxiv.org/pdf/1803.07130.pdf
countChars :: ByteString -> Int
countChars = T.length . T.toUpper . T.decodeUtf8
