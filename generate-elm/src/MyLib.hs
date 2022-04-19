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

import Data.Maybe

import Language.Haskell.To.Elm
import Language.Elm.Definition (Definition)
import qualified Language.Elm.Pretty as Pretty
import qualified Language.Elm.Simplification as Simplification

elmDefsFor :: forall a. (HasElmDecoder Aeson.Value a, HasElmEncoder Aeson.Value a) => Maybe (Definition, Definition, Definition)
elmDefsFor = (,,) <$> elmDefinition @a <*> elmEncoderDefinition @Aeson.Value @a <*> elmDecoderDefinition @Aeson.Value @a

(elmDefs, elmEncoders, elmDecoders) = unzip3 $ catMaybes 
    [ elmDefsFor @Ast.Unique
    , elmDefsFor @Ast.ExternalName
    , elmDefsFor @Ast.Binder
    , elmDefsFor @Ast.IdInfo
    , elmDefsFor @Ast.Unfolding
    , elmDefsFor @Ast.OccInfo
    , elmDefsFor @Ast.IdDetails
    , elmDefsFor @Ast.Lit
    , elmDefsFor @Ast.TyCon
    , elmDefsFor @Ast.Type
    , elmDefsFor @Ast.Module
    , elmDefsFor @Ast.Expr
    , elmDefsFor @Ast.Alt
    , elmDefsFor @Ast.AltCon
    , elmDefsFor @Ast.LineCol
    , elmDefsFor @Ast.SrcSpan
    , elmDefsFor @Ast.Tick
    , elmDefsFor @Ast.CoreStats
    , elmDefsFor @Ast.TopBinder
    , elmDefsFor @Ast.TopBinding
    ]

someFunc :: IO ()
someFunc = do
    let defs = Pretty.modules (map Simplification.simplifyDefinition elmDefs)
    forM_ (HashMap.toList defs) $ \(moduleName, contents) ->
        writeFile "Types.elm" (show contents)
    let encoders = Pretty.modules (map Simplification.simplifyDefinition elmEncoders)
    forM_ (HashMap.toList encoders) $ \(moduleName, contents) ->
        writeFile "Encoders.elm" (show contents)
    let decoders = Pretty.modules (map Simplification.simplifyDefinition elmDecoders)
    forM_ (HashMap.toList decoders) $ \(moduleName, contents) ->
        writeFile "Decoders.elm" (show contents)
    putStrLn "All done!"

    
