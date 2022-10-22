{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module HsComprehension.ElmDeriving where

import HsComprehension.Ast
import HsComprehension.ElmDerivingUtils 

import GHC.Generics
import qualified Generics.SOP as SOP
import Data.Aeson as Aeson
import Language.Haskell.To.Elm

deriving instance SOP.Generic Capture
deriving instance SOP.HasDatatypeInfo Capture
type CaptureElm = ElmType "Generated.TODO.Capture" Capture
deriving via CaptureElm instance Aeson.ToJSON Capture
deriving via CaptureElm instance HasElmType Capture
deriving via CaptureElm instance HasElmDecoder Aeson.Value Capture

deriving instance SOP.Generic ModuleMeta
deriving instance SOP.HasDatatypeInfo ModuleMeta
type ModuleMetaElm = ElmType "Generated.TODO.ModuleMeta" ModuleMeta
deriving via ModuleMetaElm instance Aeson.ToJSON ModuleMeta
deriving via ModuleMetaElm instance HasElmType ModuleMeta
deriving via ModuleMetaElm instance HasElmDecoder Aeson.Value ModuleMeta

deriving instance SOP.Generic ExternalName
deriving instance SOP.HasDatatypeInfo ExternalName
type ExternalNameElm = ElmType "Generated.TODO.ExternalName" ExternalName
deriving via ExternalNameElm instance Aeson.ToJSON ExternalName
deriving via ExternalNameElm instance HasElmType ExternalName
deriving via ExternalNameElm instance HasElmDecoder Aeson.Value ExternalName

deriving instance SOP.Generic Binder
deriving instance SOP.HasDatatypeInfo Binder
type BinderElm = ElmType "Generated.TODO.Binder" Binder
deriving via BinderElm instance Aeson.ToJSON Binder
deriving via BinderElm instance HasElmType Binder
deriving via BinderElm instance HasElmDecoder Aeson.Value Binder

deriving instance SOP.Generic IdInfo
deriving instance SOP.HasDatatypeInfo IdInfo
type IdInfoElm = ElmType "Generated.TODO.IdInfo" IdInfo
deriving via IdInfoElm instance Aeson.ToJSON IdInfo
deriving via IdInfoElm instance HasElmType IdInfo
deriving via IdInfoElm instance HasElmDecoder Aeson.Value IdInfo

deriving instance SOP.Generic Unfolding
deriving instance SOP.HasDatatypeInfo Unfolding
type UnfoldingElm = ElmType "Generated.TODO.Unfolding" Unfolding
deriving via UnfoldingElm instance Aeson.ToJSON Unfolding
deriving via UnfoldingElm instance HasElmType Unfolding
deriving via UnfoldingElm instance HasElmDecoder Aeson.Value Unfolding

deriving instance SOP.Generic Lit
deriving instance SOP.HasDatatypeInfo Lit
type LitElm = ElmType "Generated.TODO.Lit" Lit
deriving via LitElm instance Aeson.ToJSON Lit
deriving via LitElm instance HasElmType Lit
deriving via LitElm instance HasElmDecoder Aeson.Value Lit

deriving instance SOP.Generic Type
deriving instance SOP.HasDatatypeInfo Type
type TypeElm = ElmType "Generated.TODO.Type" Type
deriving via TypeElm instance Aeson.ToJSON Type
deriving via TypeElm instance HasElmType Type
deriving via TypeElm instance HasElmDecoder Aeson.Value Type

deriving instance SOP.Generic TyLit
deriving instance SOP.HasDatatypeInfo TyLit
type TyLitElm = ElmType "Generated.TODO.TyLit" TyLit
deriving via TyLitElm instance Aeson.ToJSON TyLit
deriving via TyLitElm instance HasElmType TyLit
deriving via TyLitElm instance HasElmDecoder Aeson.Value TyLit

deriving instance SOP.Generic FiredRule
deriving instance SOP.HasDatatypeInfo FiredRule
type FiredRuleElm = ElmType "Generated.TODO.FiredRule" FiredRule
deriving via FiredRuleElm instance Aeson.ToJSON FiredRule
deriving via FiredRuleElm instance HasElmType FiredRule
deriving via FiredRuleElm instance HasElmDecoder Aeson.Value FiredRule

deriving instance SOP.Generic Phase
deriving instance SOP.HasDatatypeInfo Phase
type PhaseElm = ElmType "Generated.TODO.Phase" Phase
deriving via PhaseElm instance Aeson.ToJSON Phase
deriving via PhaseElm instance HasElmType Phase
deriving via PhaseElm instance HasElmDecoder Aeson.Value Phase

deriving instance SOP.Generic Expr
deriving instance SOP.HasDatatypeInfo Expr
type ExprElm = ElmType "Generated.TODO.Expr" Expr
deriving via ExprElm instance Aeson.ToJSON Expr
deriving via ExprElm instance HasElmType Expr
deriving via ExprElm instance HasElmDecoder Aeson.Value Expr

deriving instance SOP.Generic Alt
deriving instance SOP.HasDatatypeInfo Alt
type AltElm = ElmType "Generated.TODO.Alt" Alt
deriving via AltElm instance Aeson.ToJSON Alt
deriving via AltElm instance HasElmType Alt
deriving via AltElm instance HasElmDecoder Aeson.Value Alt

deriving instance SOP.Generic AltCon
deriving instance SOP.HasDatatypeInfo AltCon
type AltConElm = ElmType "Generated.TODO.AltCon" AltCon
deriving via AltConElm instance Aeson.ToJSON AltCon
deriving via AltConElm instance HasElmType AltCon
deriving via AltConElm instance HasElmDecoder Aeson.Value AltCon

deriving instance SOP.Generic TopBindingInfo
deriving instance SOP.HasDatatypeInfo TopBindingInfo
type TopBindingInfoElm = ElmType "Generated.TODO.TopBindingInfo" TopBindingInfo
deriving via TopBindingInfoElm instance Aeson.ToJSON TopBindingInfo
deriving via TopBindingInfoElm instance HasElmType TopBindingInfo
deriving via TopBindingInfoElm instance HasElmDecoder Aeson.Value TopBindingInfo

deriving instance SOP.Generic TopBinding
deriving instance SOP.HasDatatypeInfo TopBinding
type TopBindingElm = ElmType "Generated.TODO.TopBinding" TopBinding
deriving via TopBindingElm instance Aeson.ToJSON TopBinding
deriving via TopBindingElm instance HasElmType TopBinding
deriving via TopBindingElm instance HasElmDecoder Aeson.Value TopBinding

deriving instance SOP.Generic IdDetails
deriving instance SOP.HasDatatypeInfo IdDetails
type IdDetailsElm = ElmType "Generated.TODO.IdDetails" IdDetails
deriving via IdDetailsElm instance Aeson.ToJSON IdDetails
deriving via IdDetailsElm instance HasElmType IdDetails
deriving via IdDetailsElm instance HasElmDecoder Aeson.Value IdDetails

deriving instance SOP.Generic BinderId
deriving instance SOP.HasDatatypeInfo BinderId
type BinderIdElm = ElmType "Generated.TODO.BinderId" BinderId
deriving via BinderIdElm instance Aeson.ToJSON BinderId
deriving via BinderIdElm instance HasElmType BinderId
deriving via BinderIdElm instance HasElmDecoder Aeson.Value BinderId

deriving instance SOP.Generic TyCon
deriving instance SOP.HasDatatypeInfo TyCon
type TyConElm = ElmType "Generated.TODO.TyCon" TyCon
deriving via TyConElm instance Aeson.ToJSON TyCon
deriving via TyConElm instance HasElmType TyCon
deriving via TyConElm instance HasElmDecoder Aeson.Value TyCon

deriving instance SOP.Generic SrcSpan
deriving instance SOP.HasDatatypeInfo SrcSpan
type SrcSpanElm = ElmType "Generated.TODO.SrcSpan" SrcSpan
deriving via SrcSpanElm instance Aeson.ToJSON SrcSpan
deriving via SrcSpanElm instance HasElmType SrcSpan
deriving via SrcSpanElm instance HasElmDecoder Aeson.Value SrcSpan

deriving instance SOP.Generic LineCol
deriving instance SOP.HasDatatypeInfo LineCol
type LineColElm = ElmType "Generated.TODO.LineCol" LineCol
deriving via LineColElm instance Aeson.ToJSON LineCol
deriving via LineColElm instance HasElmType LineCol
deriving via LineColElm instance HasElmDecoder Aeson.Value LineCol

deriving instance SOP.Generic OccInfo
deriving instance SOP.HasDatatypeInfo OccInfo
type OccInfoElm = ElmType "Generated.TODO.OccInfo" OccInfo
deriving via OccInfoElm instance Aeson.ToJSON OccInfo
deriving via OccInfoElm instance HasElmType OccInfo
deriving via OccInfoElm instance HasElmDecoder Aeson.Value OccInfo

deriving instance SOP.Generic Tick
deriving instance SOP.HasDatatypeInfo Tick
type TickElm = ElmType "Generated.TODO.Tick" Tick
deriving via TickElm instance Aeson.ToJSON Tick
deriving via TickElm instance HasElmType Tick
deriving via TickElm instance HasElmDecoder Aeson.Value Tick

deriving instance SOP.Generic CoreStats
deriving instance SOP.HasDatatypeInfo CoreStats
type CoreStatsElm = ElmType "Generated.TODO.CoreStats" CoreStats
deriving via CoreStatsElm instance Aeson.ToJSON CoreStats
deriving via CoreStatsElm instance HasElmType CoreStats
deriving via CoreStatsElm instance HasElmDecoder Aeson.Value CoreStats
