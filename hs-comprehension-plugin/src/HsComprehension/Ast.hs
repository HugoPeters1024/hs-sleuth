{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module HsComprehension.Ast where

import GHC.Generics
import GHC.TypeLits (Symbol)

import Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.ByteString as BS
import qualified Generics.SOP as SOP
import Language.Haskell.To.Elm

import qualified GhcDump.Ast as GD
import qualified GhcDump.Convert as GD

import Data.Text (Text)
import HsComprehension.ElmDeriving

data Unique = Unique Char Int
    deriving (Generic, SOP.Generic, SOP.HasDatatypeInfo)
    deriving ( Aeson.ToJSON
             , Aeson.FromJSON
             , HasElmType
             , HasElmDecoder Aeson.Value
             , HasElmEncoder Aeson.Value) via ElmType "Generated.HsCore.Unique" Unique
    
type ModuleName = Text

data ExternalName = ExternalName { externalModuleName :: !ModuleName
                                 , externalName :: !Text
                                 , externalUnique :: Unique
                                 } 
                                 | ForeignCall
    deriving (Generic, SOP.Generic, SOP.HasDatatypeInfo)
    deriving ( Aeson.ToJSON
             , Aeson.FromJSON
             , HasElmType
             , HasElmDecoder Aeson.Value
             , HasElmEncoder Aeson.Value) via ElmType "Generated.HsCore.ExternalName" ExternalName

type BinderId = Unique

data Binder = Binder { binderName   :: !Text
                     , binderId     :: !BinderId
                     , binderIdInfo :: IdInfo
                     , binderIdDetails :: IdDetails
                     , binderType   :: Type
                     }
            | TyBinder { binderName :: !Text
                       , binderId   :: !BinderId
                       , binderKind :: Type
                       }
    deriving (Generic, SOP.Generic, SOP.HasDatatypeInfo)
    deriving ( Aeson.ToJSON
             , Aeson.FromJSON
             , HasElmType
             , HasElmDecoder Aeson.Value
             , HasElmEncoder Aeson.Value) via ElmType "Generated.HsCore.Binder" Binder

data IdInfo
    = IdInfo { idiArity         :: !Int
             , idiIsOneShot     :: Bool
             , idiUnfolding     :: Unfolding
             , idiInlinePragma  :: !Text
             , idiOccInfo       :: OccInfo
             , idiStrictnessSig :: !Text
             , idiDemandSig     :: !Text
             , idiCallArity     :: !Int
             }
    deriving (Generic, SOP.Generic, SOP.HasDatatypeInfo)
    deriving ( Aeson.ToJSON
             , Aeson.FromJSON
             , HasElmType
             , HasElmDecoder Aeson.Value
             , HasElmEncoder Aeson.Value) via ElmType "Generated.HsCore.IdInfo" IdInfo

data Unfolding
    = NoUnfolding
    | BootUnfolding
    | OtherCon [AltCon]
    | DFunUnfolding
    | CoreUnfolding { unfTemplate   :: Expr
                    , unfIsValue    :: Bool
                    , unfIsConLike  :: Bool
                    , unfIsWorkFree :: Bool
                    , unfGuidance   :: Text
                    }
    deriving (Generic, SOP.Generic, SOP.HasDatatypeInfo)
    deriving ( Aeson.ToJSON
             , Aeson.FromJSON
             , HasElmType
             , HasElmDecoder Aeson.Value
             , HasElmEncoder Aeson.Value) via ElmType "Generated.HsCore.Unfolding" Unfolding

data OccInfo = OccManyOccs -- | introduced in GHC 8.2
             | OccDead
             | OccOneOcc
             | OccLoopBreaker { occStrongLoopBreaker :: Bool }
    deriving (Generic, SOP.Generic, SOP.HasDatatypeInfo)
    deriving ( Aeson.ToJSON
             , Aeson.FromJSON
             , HasElmType
             , HasElmDecoder Aeson.Value
             , HasElmEncoder Aeson.Value) via ElmType "Generated.HsCore.OccInfo" OccInfo

data IdDetails = VanillaId
               | RecSelId
               | DataConWorkId
               | DataConWrapId
               | ClassOpId
               | PrimOpId
               -- | FCallId  (these are treated as ExternalNames since they have no binding site)
               | TickBoxOpId
               | DFunId
               | CoVarId -- | introduced in GHC 8.0
               | JoinId { joinIdArity :: !Int }
    deriving (Generic, SOP.Generic, SOP.HasDatatypeInfo)
    deriving ( Aeson.ToJSON
             , Aeson.FromJSON
             , HasElmType
             , HasElmDecoder Aeson.Value
             , HasElmEncoder Aeson.Value) via ElmType "Generated.HsCore.IdDetails" IdDetails

data Lit = MachChar Char
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
             , Aeson.FromJSON
             , HasElmType
             , HasElmDecoder Aeson.Value
             , HasElmEncoder Aeson.Value) via ElmType "Generated.HsCore.Lit" Lit

data TyCon = TyCon !Text !Unique
    deriving (Generic, SOP.Generic, SOP.HasDatatypeInfo)
    deriving ( Aeson.ToJSON
             , Aeson.FromJSON
             , HasElmType
             , HasElmDecoder Aeson.Value
             , HasElmEncoder Aeson.Value) via ElmType "Generated.HsCore.TyCon" TyCon

data Type
    = VarTy Binder
    | FunTy Type Type
    | TyConApp TyCon [Type]
    | AppTy Type Type
    | ForAllTy Binder Type
    | LitTy
    | CoercionTy
    deriving (Generic, SOP.Generic, SOP.HasDatatypeInfo)
    deriving ( Aeson.ToJSON
             , Aeson.FromJSON
             , HasElmType
             , HasElmDecoder Aeson.Value
             , HasElmEncoder Aeson.Value) via ElmType "Generated.HsCore.Type" Type

data Module
    = Module { moduleName        :: ModuleName
             , modulePhase       :: Text
             , moduleTopBindings :: [TopBinding]
             }
    deriving (Generic, SOP.Generic, SOP.HasDatatypeInfo)
    deriving ( Aeson.ToJSON
             , Aeson.FromJSON
             , HasElmType
             , HasElmDecoder Aeson.Value
             , HasElmEncoder Aeson.Value) via ElmType "Generated.HsCore.Module" Module

data Expr
    = EVar Binder
    | EVarGlobal ExternalName
    | ELit Lit
    | EApp Expr Expr
    | ETyLam Binder Expr
    | ELam Binder Expr
    | ELet [(Binder, Expr)] Expr
    | ECase Expr Binder [Alt]
    | ETick Tick (Expr)
    | EType Type
    | ECoercion
    deriving (Generic, SOP.Generic, SOP.HasDatatypeInfo)
    deriving ( Aeson.ToJSON
             , Aeson.FromJSON
             , HasElmType
             , HasElmDecoder Aeson.Value
             , HasElmEncoder Aeson.Value) via ElmType "Generated.HsCore.Expr" Expr

data Alt = Alt { altCon     :: !AltCon
               , altBinders :: [Binder]
               , altRHS     :: Expr
               }
    deriving (Generic, SOP.Generic, SOP.HasDatatypeInfo)
    deriving ( Aeson.ToJSON
             , Aeson.FromJSON
             , HasElmType
             , HasElmDecoder Aeson.Value
             , HasElmEncoder Aeson.Value) via ElmType "Generated.HsCore.Alt" Alt

data AltCon = AltDataCon !Text
            | AltLit Lit
            | AltDefault
    deriving (Generic, SOP.Generic, SOP.HasDatatypeInfo)
    deriving ( Aeson.ToJSON
             , Aeson.FromJSON
             , HasElmType
             , HasElmDecoder Aeson.Value
             , HasElmEncoder Aeson.Value) via ElmType "Generated.HsCore.AltCon" AltCon

data LineCol = LineCol { row, column :: !Int }
    deriving (Generic, SOP.Generic, SOP.HasDatatypeInfo)
    deriving ( Aeson.ToJSON
             , Aeson.FromJSON
             , HasElmType
             , HasElmDecoder Aeson.Value
             , HasElmEncoder Aeson.Value) via ElmType "Generated.HsCore.LineCol" LineCol

data SrcSpan = SrcSpan { spanFile  :: !Text
                       , spanStart :: !LineCol
                       , spanEnd   :: !LineCol
                       }
    deriving (Generic, SOP.Generic, SOP.HasDatatypeInfo)
    deriving ( Aeson.ToJSON
             , Aeson.FromJSON
             , HasElmType
             , HasElmDecoder Aeson.Value
             , HasElmEncoder Aeson.Value) via ElmType "Generated.HsCore.SrcSpan" SrcSpan

data Tick = SourceNote { sourceTickSpan :: !SrcSpan 
                       }
    deriving (Generic, SOP.Generic, SOP.HasDatatypeInfo)
    deriving ( Aeson.ToJSON
             , Aeson.FromJSON
             , HasElmType
             , HasElmDecoder Aeson.Value
             , HasElmEncoder Aeson.Value) via ElmType "Generated.HsCore.Tick" Tick
data CoreStats
    = CoreStats { csTerms       :: !Int
                , csTypes       :: !Int
                , csCoercions   :: !Int
                , csValBinds   :: !Int
                , csJoinBinds  :: !Int
                }
    deriving (Generic, SOP.Generic, SOP.HasDatatypeInfo)
    deriving ( Aeson.ToJSON
             , Aeson.FromJSON
             , HasElmType
             , HasElmDecoder Aeson.Value
             , HasElmEncoder Aeson.Value) via ElmType "Generated.HsCore.CoreStats" CoreStats

-- So we do not need to hack triples in elm
data TopBinder = TopBinder Binder CoreStats Expr
    deriving (Generic, SOP.Generic, SOP.HasDatatypeInfo)
    deriving ( Aeson.ToJSON
             , Aeson.FromJSON
             , HasElmType
             , HasElmDecoder Aeson.Value
             , HasElmEncoder Aeson.Value) via ElmType "Generated.HsCore.TopBinder" TopBinder

data TopBinding
    = NonRecTopBinding TopBinder
    | RecTopBinding [TopBinder]
    deriving (Generic, SOP.Generic, SOP.HasDatatypeInfo)
    deriving ( Aeson.ToJSON
             , Aeson.FromJSON
             , HasElmType
             , HasElmDecoder Aeson.Value
             , HasElmEncoder Aeson.Value) via ElmType "Generated.HsCore.TopBinding" TopBinding
