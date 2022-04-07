module PlugState where

import CoreLang.Types

import GHC.Plugins
import Data.Map (Map)
import qualified Data.Map as M


data PlugState = PlugState 
    { modulePasses :: Map String [PassInfo]
    }

modName :: ModGuts -> String
modName guts = showSDocUnsafe (ppr (mg_module guts))
