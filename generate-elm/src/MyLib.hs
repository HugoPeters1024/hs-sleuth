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
import qualified HsComprehension.Meta as Meta

import Control.Monad
import qualified Data.HashMap.Lazy as HashMap

import Data.Maybe
import Data.List.Utils (replace)

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
    , elmDefsFor @Meta.ModuleMeta
    , elmDefsFor @Meta.ProjectMeta
    ]


renderDefs :: String -> [Definition] -> String
renderDefs name defs =
    let contents = show $ head $ map snd $ HashMap.toList $ Pretty.modules (map Simplification.simplifyDefinition defs)
    in replace "TODO" name contents

tripleDef :: String
tripleDef = unlines [ ""
                    , "triple : a -> b -> c -> (a,b,c)"
                    , "triple x y z = (x,y,z)"
                    ]

binderThunkDef :: String
binderThunkDef = unlines [ ""
                         , "type BinderThunk = Found Binder | NotFound | Untouched"
                         ]

finalizeTypes :: String -> String
finalizeTypes = replace ",, Binder CoreStats Expr" "Binder, CoreStats, Expr"
              . replace "BinderId Unique" "BinderId Unique (() -> BinderThunk)"
              . replace "externalType : Type }" "externalType : Type\n    , localBinder : () -> BinderThunk }"
              . (++ binderThunkDef)

finalizeDecoders :: String -> String
finalizeDecoders = replace "Tuple.triple" "triple"
                 . replace "import Json.Decode" "import Generated.Types exposing (..)\nimport Json.Decode"
                 . (++ tripleDef)


someFunc :: IO ()
someFunc = do
    writeFile "Types.elm" (finalizeTypes (renderDefs "Types" elmDefs))
    writeFile "Decoders.elm" (finalizeDecoders (renderDefs "Decoders" elmDecoders))
    putStrLn "All done!"

    
