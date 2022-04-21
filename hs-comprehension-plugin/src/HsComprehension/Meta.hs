{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
module HsComprehension.Meta where

import GHC.Generics
import Data.Aeson as Aeson
import qualified Generics.SOP as SOP
import Data.Text (Text)


import qualified HsComprehension.Ast as Ast
import HsComprehension.ElmDeriving
import Language.Haskell.To.Elm
import Codec.Serialise (Serialise)

data ModuleMeta = ModuleMeta
    { nrPasses :: Int
    , name     :: Text
    } deriving (Generic, SOP.Generic, SOP.HasDatatypeInfo, Serialise)
      deriving ( Aeson.ToJSON
               , HasElmType   
               , HasElmDecoder Aeson.Value) via ElmType "Generated.TODO.ModuleMeta" ModuleMeta

data ProjectMeta = ProjectMeta 
    { modules :: [ModuleMeta]
    } deriving (Generic, SOP.Generic, SOP.HasDatatypeInfo, Serialise)
      deriving ( Aeson.ToJSON
               , HasElmType   
               , HasElmDecoder Aeson.Value) via ElmType "Generated.TODO.ProjectMeta" ProjectMeta
