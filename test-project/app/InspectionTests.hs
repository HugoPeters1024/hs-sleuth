{-# LANGUAGE TemplateHaskell #-}

module InspectionTests where

import Test.Inspection
import qualified Text

import Tree

addNothing :: Tree Int -> Tree Int
addNothing = mapTree (+0) . mapTree identity

identity = id

countChars = Text.countChars

slice = Text.slice

inspect $ mkObligation 'countChars NoAllocation
inspect $ mkObligation 'slice      NoAllocation

