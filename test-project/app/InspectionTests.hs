{-# LANGUAGE TemplateHaskell #-}

module InspectionTests where

import Test.Inspection
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.ByteString

import Tree
--addNothing :: Tree Int -> Tree Int
--addNothing = mapTree (+0) . mapTree id

--addNothing2 :: Tree Int -> Tree Int
--addNothing2 = fmap (+0) . fmap id

countChars :: ByteString -> Int
countChars = T.length . T.toUpper . TE.decodeUtf8

inspect $ 'countChars `hasNoType` ''T.Text


