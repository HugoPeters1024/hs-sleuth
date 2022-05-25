{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module HsComprehension.Ast 
    ( Capture (..)
    , ExternalName (..)
    , Binder (..)
    , IdInfo (..)
    , Unfolding (..)
    , Lit (..)
    , Type (..)
    , Module (..)
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
    ) where

import GHC.Generics
import GHC.TypeLits (Symbol)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import Codec.Serialise (Serialise)

import GhcDump.Ast (Unique(..), IdDetails(..), BinderId(..), TyCon(..), SrcSpan(..), LineCol(..), OccInfo(..), Tick(..), CoreStats(..))

data Capture = Capture 
    { captureName :: Text
    , captureDate :: Int
    , captureModules :: [(Text, Int)]
    }
    deriving (Generic, Serialise)

data ExternalName = ExternalName
    { externalModuleName :: Text
    , externalName :: Text
    , externalUnique :: Unique
    , externalType :: Type
    }
    | ForeignCall
    deriving (Generic, Serialise)

data Binder = Binder
    { binderName :: Text
    , binderId :: BinderId
    , binderIdInfo :: IdInfo
    , binderIdDetails :: IdDetails
    , binderType :: Type
    , binderSrcSpan :: SrcSpan
    } 
    |
    TyBinder { binderName :: Text
             , binderId :: BinderId
             , binderKind :: Type
             }

    deriving (Generic, Serialise)

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
    deriving (Generic, Serialise)

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
    deriving (Generic, Serialise)

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
   deriving (Generic, Serialise)

data Type
    = VarTy BinderId
    | FunTy Type Type
    | TyConApp TyCon [Type]
    | AppTy Type Type
    | ForAllTy Binder Type
    | LitTy
    | CoercionTy
    deriving (Generic, Serialise)

data Module = Module
    { moduleName :: Text
    , modulePhase :: Text
    , modulePhaseId :: Int
    , moduleTopBindings :: [TopBinding]
    }
    deriving (Generic, Serialise)

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
    deriving (Generic, Serialise)

data Alt = Alt
    { altCon :: AltCon
    , altBinders :: [Binder]
    , altRHS :: Expr
    }
    deriving (Generic, Serialise)

data AltCon 
    = AltDataCon !T.Text
    | AltLit Lit
    | AltDefault
    deriving (Generic, Serialise)

data TopBindingInfo = TopBindingInfo
    { topBindingBinder :: Binder
    , topBindingCoreState :: CoreStats
    , topBindingRHS :: Expr
    , topBindingFromSource :: Bool
    }
    deriving (Generic, Serialise)

data TopBinding
    = NonRecTopBinding TopBindingInfo
    | RecTopBinding [TopBindingInfo]
    deriving (Generic, Serialise)

