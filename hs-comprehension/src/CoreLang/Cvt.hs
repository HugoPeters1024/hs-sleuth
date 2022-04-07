{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TupleSections #-}
module CoreLang.Cvt (cvtCoreLang) where

import CoreLang.Types

import GHC.Utils.Outputable (Outputable (..))
import qualified GHC.Core as C
import qualified GHC.Types as C
import qualified GHC.Types.Unique as C
import qualified GHC.Types.Var as C
import qualified GHC.Plugins as C

import Control.Monad
import System.Random
import Data.Hashable (hash)

import qualified Data.Text as T
import Data.Text (Text)
import qualified System.IO as C

cvtCoreLang :: [C.CoreBind] -> C.CoreM [CoreBind]
cvtCoreLang bs = mapM coreLangBind bs

pprText :: Outputable a => a -> C.CoreM Text
pprText el = do
    dflags <- C.getDynFlags 
    pure $ T.pack $ C.showPpr dflags el

coreLangId :: C.Var -> C.CoreM CoreId
coreLangId var = do
    name <- pprText (C.occName var)
    uniquetag <- pprText (C.getUnique var)
    vartype <- pprText (C.varType var)
    let (_, unique) = C.unpkUnique (C.getUnique var)
    let istyvar = C.isTyVar var
    pure $ CoreId {..}

coreLangBndr :: C.CoreBndr -> C.CoreM CoreId
coreLangBndr bndr = coreLangId bndr

coreLangLiteral :: C.Literal -> C.CoreM CoreLiteral
coreLangLiteral l@(C.LitNumber _ i) = CoreLitNumber <$> pprText l
coreLangLiteral l@(C.LitString _) = CoreLitString <$> pprText l
coreLangLiteral l = CoreLitOther <$> pprText l

coreLangBind :: C.CoreBind -> C.CoreM CoreBind
coreLangBind (C.NonRec b e) = NonRec <$> coreLangBndr b <*> coreLangExpr e
coreLangBind (C.Rec [(b, e)]) = NonRec <$> coreLangBndr b <*> coreLangExpr e

coreLangAltCon :: C.AltCon -> C.CoreM CoreAltCon
coreLangAltCon con@(C.DataAlt _) = DataAlt <$> pprText con
coreLangAltCon (C.LitAlt l) = LitAlt <$> coreLangLiteral l
coreLangAltCon (C.DEFAULT) = pure $ DEFAULT

coreLangAlt :: C.CoreAlt -> C.CoreM CoreAlt
coreLangAlt (C.Alt con bs e) = Alt <$> coreLangAltCon con <*> mapM coreLangBndr bs <*> coreLangExpr e

coreLangExpr :: C.CoreExpr -> C.CoreM CoreTerm
coreLangExpr (C.Var i) = do
    Var <$> coreLangId i
coreLangExpr (C.Lit l) = Lit <$> coreLangLiteral l
coreLangExpr (C.App e a) = App <$> coreLangExpr e <*> coreLangExpr a
coreLangExpr (C.Lam b e) = Lam <$> coreLangBndr b <*> coreLangExpr e
coreLangExpr (C.Let b e) = Let <$> coreLangBind b <*> coreLangExpr e
coreLangExpr (C.Case e _ _ alts) = Case <$> coreLangExpr e <*> mapM coreLangAlt (reverse alts)
coreLangExpr (C.Cast e c) = Cast <$> coreLangExpr e <*> pprText c
coreLangExpr (C.Coercion c) = Coercion <$> pprText c
coreLangExpr (C.Tick _ e) = coreLangExpr e
coreLangExpr (C.Type t) = Type <$> pprText t

