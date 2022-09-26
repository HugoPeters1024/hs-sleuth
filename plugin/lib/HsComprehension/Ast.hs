{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

module HsComprehension.Ast
    ( Capture (..)
    , ModuleMeta (..)
    , ExternalName (..)
    , Binder (..)
    , IdInfo (..)
    , Unfolding (..)
    , Lit (..)
    , Type (..)
    , FiredRule (..)
    , Phase (..)
    , Expr (..)
    , Alt (..)
    , AltCon (..)
    , TopBindingInfo (..)
    , TopBinding (..)
    -- re-export
    , Unique (..)
    , IdDetails (..)
    , BinderId (..)
    , TyCon (..)
    , SrcSpan (..)
    , LineCol (..)
    , OccInfo (..)
    , Tick (..)
    , CoreStats (..)
    , TyLit (..)
    , get_binderIdUniqueNum
    , get_binderUniqueNum
    , get_binderName
    , getPhaseTopLevels
    ) where

import GHC.Generics
import GHC.TypeLits (Symbol)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Codec.Serialise (Serialise)
import Data.Hashable

import Data.Map (Map)

import GhcDump.Ast (Unique(..), IdDetails(..), TyCon(..), SrcSpan(..), LineCol(..), OccInfo(..), Tick(..), CoreStats(..), TyLit(..))

data Capture = Capture
    { captureName :: Text
    , captureDate :: Int
    , captureGhcVersion :: Text
    , captureModules :: [(Text, Int)]
    }
    deriving (Generic, Serialise, Show)

data ModuleMeta = ModuleMeta
  { toplevels :: Map Text Text
  } deriving (Generic, Serialise, Show)

data ExternalName = ExternalName
    { externalModuleName :: Text
    , externalName :: Text
    , externalUnique :: Unique
    , externalType :: Type
    }
    | ForeignCall
    deriving (Generic, Serialise, Show)

data BinderId = BinderId
    { binderIdUnique :: Unique
    , binderIdRenderedUnique :: Text
    , binderIdDeBruijn :: Int
    } deriving (Generic, Serialise, Show)

get_uniqueNum :: Unique -> Int
get_uniqueNum (Unique _ i) = i

get_binderIdUniqueNum :: BinderId -> Int
get_binderIdUniqueNum = get_uniqueNum . binderIdUnique

instance Eq BinderId where
    lhs == rhs = binderIdUnique lhs == binderIdUnique rhs

instance Ord BinderId where
    compare lhs rhs = compare (binderIdUnique lhs) (binderIdUnique rhs)

data Binder = Binder
    { binderName :: Text
    , binderId :: BinderId
    , binderIdInfo :: IdInfo
    , binderIdDetails :: IdDetails
    , binderType :: Type
    , binderSrcSpan :: SrcSpan
    , binderPhaseId :: Int
    , binderCreatedPhaseId :: Int
    }
    |
    TyBinder { binderName :: Text
             , binderId :: BinderId
             , binderKind :: Type
             , binderPhaseId :: Int
             }

    deriving (Generic, Serialise, Show)

get_binderName :: Binder -> Text
get_binderName (Binder {..}) = binderName
get_binderName (TyBinder {..}) = binderName

get_binderUniqueNum :: Binder -> Int
get_binderUniqueNum (Binder {..}) = get_binderIdUniqueNum binderId
get_binderUniqueNum (TyBinder {..}) = get_binderIdUniqueNum binderId

data IdInfo = IdInfo
    { idiArity         :: !Int
    , idiIsOneShot     :: Bool
    , idiUnfolding     :: Unfolding
    , idiInlinePragma  :: !T.Text
    , idiOccInfo       :: OccInfo
    , idiStrictnessSig :: !T.Text
    , idiDemandSig     :: !T.Text
    , idiCallArity     :: !Int
    }
    deriving (Generic, Serialise, Show)

data Unfolding
    = NoUnfolding
    | BootUnfolding
    | OtherCon [AltCon]
    | DFunUnfolding
    | CoreUnfolding { unfTemplate   :: Expr
                    , unfIsValue    :: Bool
                    , unfIsConLike  :: Bool
                    , unfIsWorkFree :: Bool
                    , unfGuidance   :: T.Text
                    }
    deriving (Generic, Serialise, Show)

data Lit
    = MachChar Char
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
   deriving (Generic, Serialise, Show, Eq, Hashable)

data Type
    = VarTy BinderId
    | FunTy Type Type
    | TyConApp TyCon [Type]
    | AppTy Type Type
    | ForAllTy Binder Type
    | LitTy TyLit
    | CoercionTy
    deriving (Generic, Serialise, Show)

instance Hashable TyLit

data FiredRule = FiredRule
    { firedRuleName :: Text
    , firedRuleModule :: Text
    , firedRulePhase :: Int
    } deriving (Generic, Serialise, Show)

data Phase = Phase
    { phaseName :: Text
    , phaseId :: Int
    , phaseTopBindings :: [TopBinding]
    , phaseFiredRules :: [FiredRule]
    }
    deriving (Generic, Serialise, Show)

data Expr
    = EVar BinderId
    | EVarGlobal ExternalName
    | ELit Lit
    | EApp Expr Expr
    | ETyLam Binder Expr
    | ELam Binder Expr
    | ELet [(Binder, Expr)] Expr
    | ECase Expr Binder [Alt]
    | ETick Tick Expr
    | EType Type
    | ECoercion
    -- Marker tokens
    | EMarkDiff Expr
    deriving (Generic, Serialise, Show)

data Alt = Alt
    { altCon :: AltCon
    , altBinders :: [Binder]
    , altRHS :: Expr
    }
    deriving (Generic, Serialise, Show)

data AltCon
    = AltDataCon !T.Text
    | AltLit Lit
    | AltDefault
    deriving (Generic, Serialise, Show)

data TopBindingInfo = TopBindingInfo
    { topBindingBinder :: Binder
    , topBindingCoreState :: CoreStats
    , topBindingRHS :: Expr
    , topBindingFromSource :: Bool
    , topBindingHash :: Int
    }
    deriving (Generic, Serialise, Show)

data TopBinding
    = NonRecTopBinding TopBindingInfo
    | RecTopBinding [TopBindingInfo]
    deriving (Generic, Serialise, Show)

getPhaseTopLevels :: Phase -> [TopBindingInfo]
getPhaseTopLevels phase = concatMap go (phaseTopBindings phase)
  where go :: TopBinding -> [TopBindingInfo]
        go (NonRecTopBinding ti) = [ti]
        go (RecTopBinding tis) = tis

