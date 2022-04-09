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


type PlugState =  Map String (ModuleInfo, [PassInfo])

updateMapUnsafe :: (Ord k) => (a -> a) -> k -> Map k a -> Map k a
updateMapUnsafe f k m = case M.lookup k m of
                          Nothing -> error "key does not exist"
                          Just v -> M.update (const (Just (f v))) k m


addPass :: String -> PassInfo -> PlugState -> PlugState
addPass modName pass state = updateMapUnsafe (\(m, passes) -> (m{nrpasses = m.nrpasses + 1}, passes++[pass])) modName state

setSrcBindings :: String -> [Int] -> PlugState -> PlugState
setSrcBindings modname binds m = updateMapUnsafe (\(m, passes) -> (m{srcbindings = binds},passes)) modname m

modName :: ModGuts -> String
modName guts = showSDocUnsafe (ppr (mg_module guts))
