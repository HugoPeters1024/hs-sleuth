module Text where

import Data.Text (Text)
import qualified Data.Text as T

upperLength :: Text -> Text
upperLength = T.take 3 . T.drop 4
