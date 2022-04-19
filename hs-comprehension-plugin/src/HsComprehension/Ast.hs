{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module HsComprehension.Ast 
    ( Unique(..)
    , SExternalName (..)
    , BinderId (..)
    , SBinderG (..)
    , SBinder (..)
    , SIdInfo (..)
    , SUnfolding (..)
    , OccInfo (..)
    , IdDetails (..)
    , Ast.Lit(..)
    , TyCon (..)
    , SType (..)
    , ModuleName (..)
    , SModule (..)
    , SExpr (..)
    , SAlt (..)
    , AltCon (..)
    , LineCol (..)
    , SrcSpan (..)
    , Tick (..)
    , STopBinding (..)
    , CoreStats (..)
    , writeSModule
    , readSModule
    ) where

import GHC.Generics
import GHC.TypeLits (Symbol)

import Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Generics.SOP as SOP
import Language.Haskell.To.Elm

import Data.Text (Text)
import HsComprehension.ElmDeriving

import GhcDump.Ast hiding (Lit(..))
import qualified GhcDump.Ast as Ast

type Elm a = (Generic a, SOP.Generic a, SOP.HasDatatypeInfo a, Aeson.ToJSON a, HasElmType a, HasElmDecoder Aeson.Value a)

-- Simplified Lit that is parsable to json
-- used a proxy type for the actual lit
data ProxyLit = MachChar Char
              | MachStr Text
              | MachNullAddr
              | MachInt Text
              | MachInt64 Text
              | MachWord Text
              | MachWord64 Text
              | MachFloat Text
              | MachDouble Text
              | MachLabel Text
              | LitInteger Text
              | LitNatural Text
              | LitRubbish
              deriving (Generic, SOP.Generic, SOP.HasDatatypeInfo)
              deriving ( Aeson.ToJSON
                       , HasElmType   
                       , HasElmDecoder Aeson.Value
                       , HasElmEncoder Aeson.Value) via ElmType "Generated.TODO.Lit" ProxyLit

showText :: Show a => a -> Text
showText = T.pack . show

-- Convertible instance to allow proxy deriving
instance Convertible Ast.Lit ProxyLit where
    convert (Ast.MachChar c) = MachChar c
    convert (Ast.MachStr b) = MachStr (showText b)
    convert (Ast.MachNullAddr) = MachNullAddr
    convert (Ast.MachInt i) = MachInt (showText i)
    convert (Ast.MachInt64 i) = MachInt64 (showText i)
    convert (Ast.MachWord i) = MachWord (showText i)
    convert (Ast.MachWord64 i) = MachWord64 (showText i)
    convert (Ast.MachFloat f) = MachFloat (showText f)
    convert (Ast.MachDouble d) = MachDouble (showText d)
    convert (Ast.MachLabel l) = MachLabel (showText l)
    convert (Ast.LitInteger i) = LitInteger (showText i)
    convert (Ast.LitNatural n) = LitNatural (showText n)
    convert (Ast.LitRubbish) = LitRubbish

deriving instance SOP.Generic Unique
deriving instance SOP.HasDatatypeInfo Unique
deriving via ElmType "Generated.TODO.Unique" Unique instance Aeson.ToJSON Unique
deriving via ElmType "Generated.TODO.Unique" Unique instance HasElmType Unique
deriving via ElmType "Generated.TODO.Unique" Unique instance HasElmDecoder Aeson.Value Unique

deriving instance SOP.Generic SExternalName
deriving instance SOP.HasDatatypeInfo SExternalName
deriving via ElmType "Generated.TODO.ExternalName" SExternalName instance Aeson.ToJSON SExternalName
deriving via ElmType "Generated.TODO.ExternalName" SExternalName instance HasElmType SExternalName
deriving via ElmType "Generated.TODO.ExternalName" SExternalName instance HasElmDecoder Aeson.Value SExternalName


deriving instance Generic BinderId
deriving instance SOP.Generic BinderId
deriving instance SOP.HasDatatypeInfo BinderId
deriving via ElmType "Generated.TODO.BinderId" BinderId instance Aeson.ToJSON BinderId
deriving via ElmType "Generated.TODO.BinderId" BinderId instance HasElmType BinderId
deriving via ElmType "Generated.TODO.BinderId" BinderId instance HasElmDecoder Aeson.Value BinderId

type SBinderG = Binder' SBinder BinderId
deriving instance SOP.Generic SBinderG
deriving instance SOP.HasDatatypeInfo SBinderG
deriving via ElmType "Generated.TODO.Binder" SBinderG instance Aeson.ToJSON SBinderG
deriving via ElmType "Generated.TODO.Binder" SBinderG instance HasElmType SBinderG
deriving via ElmType "Generated.TODO.Binder" SBinderG instance HasElmDecoder Aeson.Value SBinderG

instance Convertible SBinder (Binder' SBinder BinderId)
    where convert (SBndr x) = x

deriving instance SOP.Generic SBinder
deriving instance SOP.HasDatatypeInfo SBinder
deriving via ElmProxyType "Generated.TODO.Binder" SBinder SBinderG instance Aeson.ToJSON SBinder
deriving via ElmProxyType "Generated.TODO.Binder" SBinder SBinderG instance HasElmType SBinder
deriving via ElmProxyType "Generated.TODO.Binder" SBinder SBinderG instance HasElmDecoder Aeson.Value SBinder

type SIdInfo = IdInfo SBinder BinderId
deriving instance SOP.Generic SIdInfo
deriving instance SOP.HasDatatypeInfo SIdInfo
deriving via ElmType "Generated.TODO.IdInfo" SIdInfo instance Aeson.ToJSON SIdInfo
deriving via ElmType "Generated.TODO.IdInfo" SIdInfo instance HasElmType SIdInfo
deriving via ElmType "Generated.TODO.IdInfo" SIdInfo instance HasElmDecoder Aeson.Value SIdInfo

type SUnfolding = Unfolding SBinder BinderId
deriving instance SOP.Generic SUnfolding
deriving instance SOP.HasDatatypeInfo SUnfolding
deriving via ElmType "Generated.TODO.Unfolding" SUnfolding instance Aeson.ToJSON SUnfolding
deriving via ElmType "Generated.TODO.Unfolding" SUnfolding instance HasElmType SUnfolding
deriving via ElmType "Generated.TODO.Unfolding" SUnfolding instance HasElmDecoder Aeson.Value SUnfolding

deriving instance SOP.Generic OccInfo
deriving instance SOP.HasDatatypeInfo OccInfo
deriving via ElmType "Generated.TODO.OccInfo" OccInfo instance Aeson.ToJSON OccInfo
deriving via ElmType "Generated.TODO.OccInfo" OccInfo instance HasElmType OccInfo
deriving via ElmType "Generated.TODO.OccInfo" OccInfo instance HasElmDecoder Aeson.Value OccInfo

deriving instance SOP.Generic IdDetails
deriving instance SOP.HasDatatypeInfo IdDetails
deriving via ElmType "Generated.TODO.IdDetails" IdDetails instance Aeson.ToJSON IdDetails
deriving via ElmType "Generated.TODO.IdDetails" IdDetails instance HasElmType IdDetails
deriving via ElmType "Generated.TODO.IdDetails" IdDetails instance HasElmDecoder Aeson.Value IdDetails

deriving via ElmProxyType "Generated.TODO.Lit" Ast.Lit ProxyLit instance Aeson.ToJSON Ast.Lit
deriving via ElmProxyType "Generated.TODO.Lit" Ast.Lit ProxyLit instance HasElmType Ast.Lit
deriving via ElmProxyType "Generated.TODO.Lit" Ast.Lit ProxyLit instance HasElmDecoder Aeson.Value Ast.Lit

deriving instance SOP.Generic TyCon
deriving instance SOP.HasDatatypeInfo TyCon
deriving via ElmType "Generated.TODO.TyCon" TyCon instance Aeson.ToJSON TyCon
deriving via ElmType "Generated.TODO.TyCon" TyCon instance HasElmType TyCon
deriving via ElmType "Generated.TODO.TyCon" TyCon instance HasElmDecoder Aeson.Value TyCon

deriving instance SOP.Generic SType
deriving instance SOP.HasDatatypeInfo SType
deriving via ElmType "Generated.TODO.Type" SType instance Aeson.ToJSON SType
deriving via ElmType "Generated.TODO.Type" SType instance HasElmType SType
deriving via ElmType "Generated.TODO.Type" SType instance HasElmDecoder Aeson.Value SType

deriving instance Generic ModuleName
deriving instance SOP.Generic ModuleName
deriving instance SOP.HasDatatypeInfo ModuleName
deriving via ElmType "Generated.TODO.ModuleName" ModuleName instance Aeson.ToJSON ModuleName
deriving via ElmType "Generated.TODO.ModuleName" ModuleName instance HasElmType ModuleName
deriving via ElmType "Generated.TODO.ModuleName" ModuleName instance HasElmDecoder Aeson.Value ModuleName


deriving instance SOP.Generic SModule
deriving instance SOP.HasDatatypeInfo SModule
deriving via ElmType "Generated.TODO.Module" SModule instance Aeson.ToJSON SModule
deriving via ElmType "Generated.TODO.Module" SModule instance HasElmType SModule
deriving via ElmType "Generated.TODO.Module" SModule instance HasElmDecoder Aeson.Value SModule

deriving instance SOP.Generic SExpr
deriving instance SOP.HasDatatypeInfo SExpr
deriving via ElmType "Generated.TODO.Expr" SExpr instance Aeson.ToJSON SExpr
deriving via ElmType "Generated.TODO.Expr" SExpr instance HasElmType SExpr
deriving via ElmType "Generated.TODO.Expr" SExpr instance HasElmDecoder Aeson.Value SExpr

deriving instance SOP.Generic SAlt
deriving instance SOP.HasDatatypeInfo SAlt
deriving via ElmType "Generated.TODO.Alt" SAlt instance Aeson.ToJSON SAlt
deriving via ElmType "Generated.TODO.Alt" SAlt instance HasElmType SAlt
deriving via ElmType "Generated.TODO.Alt" SAlt instance HasElmDecoder Aeson.Value SAlt

deriving instance SOP.Generic AltCon
deriving instance SOP.HasDatatypeInfo AltCon
deriving via ElmType "Generated.TODO.AltCon" AltCon instance Aeson.ToJSON AltCon
deriving via ElmType "Generated.TODO.AltCon" AltCon instance HasElmType AltCon
deriving via ElmType "Generated.TODO.AltCon" AltCon instance HasElmDecoder Aeson.Value AltCon

deriving instance SOP.Generic LineCol
deriving instance SOP.HasDatatypeInfo LineCol
deriving via ElmType "Generated.TODO.LineCol" LineCol instance Aeson.ToJSON LineCol
deriving via ElmType "Generated.TODO.LineCol" LineCol instance HasElmType LineCol
deriving via ElmType "Generated.TODO.LineCol" LineCol instance HasElmDecoder Aeson.Value LineCol

deriving instance SOP.Generic SrcSpan
deriving instance SOP.HasDatatypeInfo SrcSpan
deriving via ElmType "Generated.TODO.SrcSpan" SrcSpan instance Aeson.ToJSON SrcSpan
deriving via ElmType "Generated.TODO.SrcSpan" SrcSpan instance HasElmType SrcSpan
deriving via ElmType "Generated.TODO.SrcSpan" SrcSpan instance HasElmDecoder Aeson.Value SrcSpan

deriving instance SOP.Generic Tick
deriving instance SOP.HasDatatypeInfo Tick
deriving via ElmType "Generated.TODO.Tick" Tick instance Aeson.ToJSON Tick
deriving via ElmType "Generated.TODO.Tick" Tick instance HasElmType Tick
deriving via ElmType "Generated.TODO.Tick" Tick instance HasElmDecoder Aeson.Value Tick

deriving instance SOP.Generic STopBinding
deriving instance SOP.HasDatatypeInfo STopBinding
deriving via ElmType "Generated.TODO.TopBinding" STopBinding instance Aeson.ToJSON STopBinding
deriving via ElmType "Generated.TODO.TopBinding" STopBinding instance HasElmType STopBinding
deriving via ElmType "Generated.TODO.TopBinding" STopBinding instance HasElmDecoder Aeson.Value STopBinding

deriving instance SOP.Generic CoreStats
deriving instance SOP.HasDatatypeInfo CoreStats
deriving via ElmType "Generated.TODO.CoreStats" CoreStats instance Aeson.ToJSON CoreStats
deriving via ElmType "Generated.TODO.CoreStats" CoreStats instance HasElmType CoreStats
deriving via ElmType "Generated.TODO.CoreStats" CoreStats instance HasElmDecoder Aeson.Value CoreStats
