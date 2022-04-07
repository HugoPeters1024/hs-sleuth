{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module PlugState where

import CoreLang.Types

import Data.Aeson
import GHC.Generics

import GHC.Plugins
import Data.Map (Map)
import qualified Data.Map as M


type PlugState =  Map String ModuleInfo

data ModuleInfo = ModuleInfo 
    { passes :: [PassInfo]
    , srcbindings :: [Int]
    } deriving (Generic, ToJSON)

addPass :: PassInfo -> ModuleInfo -> ModuleInfo
addPass pass m = m{passes = pass:m.passes}

setSrcBindings :: [Int] -> ModuleInfo -> ModuleInfo
setSrcBindings binds m = m{srcbindings = binds}

embellishPasses :: ModuleInfo -> ModuleInfo
embellishPasses m = let
    total = length m.passes

    setNrPasses :: Int -> PassInfo -> PassInfo
    setNrPasses n pass = pass { totalpasses = n }

    setSrcBindings :: [Int] -> PassInfo -> PassInfo
    setSrcBindings bs pass = pass{srcbinders = bs}

    in m{ passes = reverse (map (setNrPasses total . setSrcBindings m.srcbindings) m.passes) 
        }


modName :: ModGuts -> String
modName guts = showSDocUnsafe (ppr (mg_module guts))
