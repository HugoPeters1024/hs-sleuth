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

elmDefsFor :: forall a. (HasElmType a, HasElmDecoder Aeson.Value a) => Maybe (Definition, Definition)
elmDefsFor = (,) <$> elmDefinition @a <*> elmDecoderDefinition @Aeson.Value @a

(elmDefs, elmDecoders) = unzip $ catMaybes 
    [ elmDefsFor @Ast.Unique
    , elmDefsFor @Ast.SExternalName
    , elmDefsFor @Ast.BinderId
    , elmDefsFor @Ast.SBinder
    , elmDefsFor @Ast.SIdInfo
    , elmDefsFor @Ast.SUnfolding
    , elmDefsFor @Ast.OccInfo
    , elmDefsFor @Ast.IdDetails
    , elmDefsFor @Ast.Lit
    , elmDefsFor @Ast.TyCon 
    , elmDefsFor @Ast.SType 
    , elmDefsFor @Ast.ModuleName 
    , elmDefsFor @Ast.SModule 
    , elmDefsFor @Ast.SExpr 
    , elmDefsFor @Ast.SAlt 
    , elmDefsFor @Ast.AltCon 
    , elmDefsFor @Ast.LineCol 
    , elmDefsFor @Ast.SrcSpan 
    , elmDefsFor @Ast.Tick 
    , elmDefsFor @Ast.STopBinding 
    , elmDefsFor @Ast.CoreStats 
    ]


someFunc :: IO ()
someFunc = do
    let defs = Pretty.modules (map Simplification.simplifyDefinition elmDefs)
    forM_ (HashMap.toList defs) $ \(moduleName, contents) ->
        writeFile "Types.elm" (show contents)
    let decoders = Pretty.modules (map Simplification.simplifyDefinition elmDecoders)
    forM_ (HashMap.toList decoders) $ \(moduleName, contents) ->
        writeFile "Decoders.elm" (show contents)
    putStrLn "All done!"

    
