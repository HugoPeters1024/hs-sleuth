{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
module HsComprehension.ElmDerivingUtils where

import Data.Proxy
import Data.String (fromString)
import GHC.Generics (Generic, Rep)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import qualified Generics.SOP as SOP

import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Map (Map)

import qualified Language.Elm.Name as Name
import qualified Language.Elm.Pretty as Pretty
import qualified Language.Elm.Simplification as Simplification
import qualified Language.Elm.Expression as Expression
import qualified Language.Elm.Pattern as Pattern
import qualified Language.Elm.Type as Type
import Language.Haskell.To.Elm
import qualified Bound

-------------------------------------------------------------------------------
-- A type to derive via, which should typically only be defined once per project.

newtype ElmProxyType (name :: Symbol) real proxy = ElmProxyType real
type ElmType (name :: Symbol) a = ElmProxyType name a a

class Convertible a b where
    convert :: a -> b

instance Convertible a a where
    convert = id

instance
  (Generic proxy, Aeson.GToJSON Aeson.Zero (Rep proxy), Convertible real proxy) =>
  Aeson.ToJSON (ElmProxyType name real proxy)
  where
  toJSON (ElmProxyType val) =
    Aeson.genericToJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = dropWhile (== '_')} (convert @real @proxy val)

instance
  (Generic proxy, Aeson.GFromJSON Aeson.Zero (Rep proxy), Convertible proxy real) =>
  Aeson.FromJSON (ElmProxyType name real proxy)
  where
  parseJSON =
    fmap (ElmProxyType . convert @proxy @real) . Aeson.genericParseJSON Aeson.defaultOptions {Aeson.fieldLabelModifier = dropWhile (== '_')}

instance
  (SOP.HasDatatypeInfo proxy, SOP.All2 HasElmType (SOP.Code proxy), KnownSymbol name, Convertible real proxy) =>
  HasElmType (ElmProxyType name real proxy)
  where
  elmDefinition =
    Just
      $ deriveElmTypeDefinition @proxy defaultOptions {fieldLabelModifier = dropWhile (== '_')}
      $ fromString $ symbolVal $ Proxy @name

instance
  (SOP.HasDatatypeInfo proxy, HasElmType proxy, SOP.All2 (HasElmDecoder Aeson.Value) (SOP.Code proxy), HasElmType (ElmProxyType name real proxy), KnownSymbol name, Convertible real proxy) =>
  HasElmDecoder Aeson.Value (ElmProxyType name real proxy)
  where
  elmDecoderDefinition =
    Just
      $ deriveElmJSONDecoder
        @proxy
        defaultOptions {fieldLabelModifier = dropWhile (== '_')}
        Aeson.defaultOptions {Aeson.fieldLabelModifier = dropWhile (== '_')}
      $ Name.Qualified moduleName $ lowerName <> "Decoder"
    where
      Name.Qualified moduleName name = fromString $ symbolVal $ Proxy @name
      lowerName = Text.toLower (Text.take 1 name) <> Text.drop 1 name

instance
  (SOP.HasDatatypeInfo proxy, HasElmType proxy, SOP.All2 (HasElmEncoder Aeson.Value) (SOP.Code proxy), HasElmType (ElmProxyType name real proxy), KnownSymbol name, Convertible real proxy) =>
  HasElmEncoder Aeson.Value (ElmProxyType name real proxy)
  where
  elmEncoderDefinition =
    Just
      $ deriveElmJSONEncoder
        @proxy
        defaultOptions {fieldLabelModifier = dropWhile (== '_')}
        Aeson.defaultOptions {Aeson.fieldLabelModifier = dropWhile (== '_')}
      $ Name.Qualified moduleName $ lowerName <> "Encoder"
    where
      Name.Qualified moduleName name = fromString $ symbolVal $ Proxy @name
      lowerName = Text.toLower (Text.take 1 name) <> Text.drop 1 name



-- Extra case for triples
instance (HasElmType a, HasElmType b, HasElmType c) => HasElmType (a, b, c) where
  elmType =
    Type.apps "Basics.,," [elmType @a, elmType @b, elmType @c]

instance (HasElmEncoder Aeson.Value a, HasElmEncoder Aeson.Value b, HasElmEncoder Aeson.Value c) => HasElmEncoder Aeson.Value (a, b, c) where
  elmEncoder =
    Expression.Lam $ Bound.toScope $
      Expression.Case (pure $ Bound.B ())
        [ ( Pattern.tuple (Pattern.Var 0) (Pattern.Var 1)
          , Bound.toScope $
            Expression.apps
              "Json.Encode.list"
              [ "Basics.identity"
              , Expression.List
                  [ Expression.App (elmEncoder @Aeson.Value @a) $ pure $ Bound.B 0
                  , Expression.App (elmEncoder @Aeson.Value @b) $ pure $ Bound.B 1
                  , Expression.App (elmEncoder @Aeson.Value @c) $ pure $ Bound.B 2
                  ]
              ]
          )
        ]

instance (HasElmDecoder Aeson.Value a, HasElmDecoder Aeson.Value b, HasElmDecoder Aeson.Value c) => HasElmDecoder Aeson.Value (a, b, c) where
  elmDecoder =
    Expression.apps
      "Json.Decode.map3"
      [ "Tuple.triple"
      , Expression.apps "Json.Decode.index" [Expression.Int 0, elmDecoder @Aeson.Value @a]
      , Expression.apps "Json.Decode.index" [Expression.Int 1, elmDecoder @Aeson.Value @b]
      , Expression.apps "Json.Decode.index" [Expression.Int 2, elmDecoder @Aeson.Value @c]
      ]

instance HasElmType v => HasElmType (Map Text v) where
  elmType = Type.apps "Dict.Dict" [elmType @Text, elmType @v]

instance (HasElmDecoder Aeson.Value v) => HasElmDecoder Aeson.Value (Map Text v) where
  elmDecoder = Expression.apps "Json.Decode.dict" [elmDecoder @Aeson.Value @v]
    
