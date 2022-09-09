{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Main where

import HsComprehension.Ast
import HsComprehension.Server.ElmDeriving

import Data.Maybe
import Data.List.Utils (replace)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Lazy as HashMap
import Language.Haskell.To.Elm
import Language.Elm.Definition (Definition)
import qualified Language.Elm.Pretty as Pretty
import qualified Language.Elm.Simplification as Simplification

replace' :: String -> String -> String -> String
replace' x y z = let r = replace x y z in if r == z then error "replace failed" else r

elmDefsFor :: forall a. (HasElmType a, HasElmDecoder Aeson.Value a) => Maybe (Definition, Definition)
elmDefsFor = (,) <$> elmDefinition @a <*> elmDecoderDefinition @Aeson.Value @a

(elmDefs, elmDecoders) = unzip $ catMaybes 
    [ elmDefsFor @ServerSettings
    , elmDefsFor @Capture
    , elmDefsFor @Unique
    , elmDefsFor @ExternalName
    , elmDefsFor @BinderId
    , elmDefsFor @Binder
    , elmDefsFor @IdInfo
    , elmDefsFor @Unfolding
    , elmDefsFor @OccInfo
    , elmDefsFor @IdDetails
    , elmDefsFor @Lit
    , elmDefsFor @TyCon 
    , elmDefsFor @Type 
    , elmDefsFor @TyLit
    , elmDefsFor @FiredRule
    , elmDefsFor @Phase
    , elmDefsFor @Module 
    , elmDefsFor @Expr 
    , elmDefsFor @Alt 
    , elmDefsFor @AltCon 
    , elmDefsFor @LineCol 
    , elmDefsFor @SrcSpan 
    , elmDefsFor @Tick 
    , elmDefsFor @TopBindingInfo
    , elmDefsFor @TopBinding 
    , elmDefsFor @CoreStats 
    ]

renderDefs :: String -> [Definition] -> String
renderDefs name defs =
    let contents = show $ head $ map snd $ HashMap.toList $ Pretty.modules (map Simplification.simplifyDefinition defs)
    in replace' "TODO" name 
     $ contents

binderThunkDef :: String
binderThunkDef = unlines [ ""
                         , "type BinderThunk = Found Binder | NotFound | Untouched"
                         ]

finalizeTypes :: String -> String
finalizeTypes = replace
                    "{ binderIdUnique : Unique"
                    "{ binderIdThunk : () -> BinderThunk,\n    binderIdUnique : Unique"
               . replace' "externalType : Type }" "externalType : Type\n    , localBinder : () -> BinderThunk }"
               . replace' "TopBinding(..)" "TopBinding(..)\n    , BinderThunk(..)"
              . (++ binderThunkDef)

finalizeDecoders :: String -> String
finalizeDecoders = replace' "import Json.Decode" "import Generated.Types exposing (..)\nimport Json.Decode"
                 . replace' "externalUnique = d" "externalUnique = d\n            , localBinder = \\_ -> Untouched"
                 . replace' 
                      "    Json.Decode.succeed BinderId |>"
                      "    Json.Decode.succeed (BinderId (\\_ -> Untouched)) |>"

main :: IO ()
main = do
  writeFile "Types.elm" (finalizeTypes (renderDefs "Types" elmDefs))
  writeFile "Decoders.elm" (finalizeDecoders (renderDefs "Decoders" elmDecoders))
  putStrLn "All done!"
