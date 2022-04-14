module Text where

import Data.Text (Text)
import qualified Data.Text as T

upperLength :: Text -> Int
upperLength = T.length . T.toUpper
