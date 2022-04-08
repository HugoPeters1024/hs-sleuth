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


addPass :: PassInfo -> ModuleInfo -> ModuleInfo
addPass pass m = m{passes = m.passes++[pass]}

setSrcBindings :: [Int] -> ModuleInfo -> ModuleInfo
setSrcBindings binds m = m{srcbindings = binds}

modName :: ModGuts -> String
modName guts = showSDocUnsafe (ppr (mg_module guts))
