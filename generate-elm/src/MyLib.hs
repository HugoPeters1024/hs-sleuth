{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module MyLib (someFunc) where

import GHC.Generics
import qualified Data.Aeson as Aeson
import qualified HsComprehension.Ast as Ast

import Control.Monad
import qualified Data.HashMap.Lazy as HashMap

import Language.Haskell.To.Elm
import Language.Elm.Definition (Definition)
import qualified Language.Elm.Pretty as Pretty
import qualified Language.Elm.Simplification as Simplification

elmDefFor :: forall a. (HasElmDecoder Aeson.Value a, HasElmEncoder Aeson.Value a) => [Definition]
elmDefFor = jsonDefinitions @a

someFunc :: IO ()
someFunc = do
    let definitions = elmDefFor @Ast.Unique
                   ++ elmDefFor @Ast.ExternalName
                   ++ elmDefFor @Ast.Binder
                   ++ elmDefFor @Ast.IdInfo
                   ++ elmDefFor @Ast.Unfolding
                   ++ elmDefFor @Ast.OccInfo
                   ++ elmDefFor @Ast.IdDetails
                   ++ elmDefFor @Ast.Lit
                   ++ elmDefFor @Ast.TyCon
                   ++ elmDefFor @Ast.Type
                   ++ elmDefFor @Ast.Module
                   ++ elmDefFor @Ast.Expr
                   ++ elmDefFor @Ast.Alt
                   ++ elmDefFor @Ast.AltCon
                   ++ elmDefFor @Ast.LineCol
                   ++ elmDefFor @Ast.SrcSpan
                   ++ elmDefFor @Ast.Tick
                   ++ elmDefFor @Ast.CoreStats
                   ++ elmDefFor @Ast.TopBinder
                   ++ elmDefFor @Ast.TopBinding
        modules = Pretty.modules (map Simplification.simplifyDefinition definitions)
    forM_ (HashMap.toList modules) $ \(moduleName, contents) -> print contents

    
