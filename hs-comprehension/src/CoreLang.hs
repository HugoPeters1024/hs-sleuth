{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module CoreLang where

import GHC.Utils.Outputable (Outputable (..))
import qualified GHC.Core as C
import qualified GHC.Types as C
import qualified GHC.Plugins as C

import qualified Data.Text as T
import Data.Text (Text)

import GHC.Generics (Generic)
import Data.Aeson
import Elm (Elm, ElmStreet (..))

data CoreLiteral = CoreLitNumber Text
                 | CoreLitString Text
                 | CoreLitOther Text -- underspecicfied catch all value
                 deriving (Show, Generic)
                 deriving (Elm, ToJSON, FromJSON) via ElmStreet CoreLiteral

data CoreBndr = CoreBndr { name :: Text
                 }
     deriving (Show, Generic)
     deriving (Elm, ToJSON, FromJSON) via ElmStreet CoreBndr


data CoreBind = NonRec CoreBndr CoreTerm
     deriving (Show, Generic)
     deriving (Elm, ToJSON, FromJSON) via ElmStreet CoreBind

data CoreAltCon = DataAlt Text    -- should only be a variable
                | LitAlt CoreLiteral 
                | DEFAULT
     deriving (Show, Generic)
     deriving (Elm, ToJSON, FromJSON) via ElmStreet CoreAltCon

data CoreAlt = Alt CoreAltCon [CoreBndr] CoreTerm
     deriving (Show, Generic)
     deriving (Elm, ToJSON, FromJSON) via ElmStreet CoreAlt

data CoreTerm
    = Var Text
    | Lit CoreLiteral
    | App CoreTerm CoreTerm
    | Lam CoreBndr CoreTerm
    | Let CoreBind CoreTerm
    | Case CoreTerm [CoreAlt]
    | Type Text
    | Undef Text
     deriving (Show, Generic)
     deriving (Elm, ToJSON, FromJSON) via ElmStreet CoreTerm

pprText :: Outputable a => a -> C.CoreM Text
pprText el = do
    dflags <- C.getDynFlags 
    pure $ T.pack $ C.showSDoc dflags $ ppr el

coreLangBndr :: Outputable b => b -> C.CoreM CoreBndr
coreLangBndr b = do
    name <- pprText b
    pure $ CoreBndr {..}

coreLangLiteral :: C.Literal -> C.CoreM CoreLiteral
coreLangLiteral l@(C.LitNumber _ i) = CoreLitNumber <$> pprText l
coreLangLiteral l@(C.LitString _) = CoreLitString <$> pprText l
coreLangLiteral l = CoreLitOther <$> pprText l

coreLangBind :: C.CoreBind -> C.CoreM CoreBind
coreLangBind (C.NonRec b e) = NonRec <$> coreLangBndr b <*> coreLangExpr e

coreLangAltCon :: C.AltCon -> C.CoreM CoreAltCon
coreLangAltCon con@(C.DataAlt _) = DataAlt <$> pprText con
coreLangAltCon (C.LitAlt l) = LitAlt <$> coreLangLiteral l
coreLangAltCon (C.DEFAULT) = pure $ DEFAULT

coreLangAlt :: C.CoreAlt -> C.CoreM CoreAlt
coreLangAlt (C.Alt con bs e) = Alt <$> coreLangAltCon con <*> mapM coreLangBndr bs <*> coreLangExpr e

coreLangExpr :: C.CoreExpr -> C.CoreM CoreTerm
coreLangExpr (C.Var i) = Var <$> pprText i
coreLangExpr (C.Lit l) = Lit <$> coreLangLiteral l
coreLangExpr (C.App e a) = App <$> coreLangExpr e <*> coreLangExpr a
coreLangExpr (C.Lam b e) = Lam <$> coreLangBndr b <*> coreLangExpr e
coreLangExpr (C.Let b e) = Let <$> coreLangBind b <*> coreLangExpr e
coreLangExpr (C.Case e _ _ alts) = Case <$> coreLangExpr e <*> mapM coreLangAlt alts
coreLangExpr (C.Cast e _) = coreLangExpr e
coreLangExpr (C.Coercion _) = pure $ Undef "Coercion"
coreLangExpr (C.Tick _ e) = coreLangExpr e
coreLangExpr (C.Type t) = Type <$> pprText t
