module TextExt where

import Data.Text as T

test :: Text -> Int
test = T.length . T.init . T.toUpper

msg :: Int
msg = test (T.pack "abcd")
