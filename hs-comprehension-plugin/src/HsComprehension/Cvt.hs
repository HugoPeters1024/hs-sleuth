{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
module HsComprehension.Cvt where

import HsComprehension.Ast
import qualified GhcDump.Ast as O --O as in origin or original

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

showText :: Show a => a -> Text
showText = T.pack . show

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x, y, z) = f x y z

cvtUnique :: O.Unique -> Unique
cvtUnique (O.Unique c i) = Unique c i

cvtModuleName :: O.ModuleName -> ModuleName
cvtModuleName (O.ModuleName x) = x

cvtExternalName :: O.SExternalName -> ExternalName
cvtExternalName o = ExternalName { externalModuleName = cvtModuleName o.externalModuleName
                                 , externalName = o.externalName
                                 , externalUnique = cvtUnique o.externalUnique
                                 , externalType = cvtType o.externalType
                                 }

cvtBinderId :: O.BinderId -> BinderId
cvtBinderId (O.BinderId u) = cvtUnique u

cvtBinder :: O.SBinder -> Binder
cvtBinder (O.SBndr o@(O.Binder {})) = Binder { binderName = o.binderName
                                             , binderId = cvtBinderId o.binderId
                                             , binderIdInfo = cvtIdInfo o.binderIdInfo
                                             , binderIdDetails = cvtIdDetails o.binderIdDetails
                                             , binderType = cvtType o.binderType
                                             }
cvtBinder (O.SBndr o@(O.TyBinder {})) = TyBinder { binderName = o.binderName
                                                 , binderId = cvtBinderId o.binderId
                                                 , binderKind = cvtType o.binderKind
                                                 }

cvtIdInfo :: O.IdInfo O.SBinder O.BinderId -> IdInfo
cvtIdInfo o = IdInfo { idiArity = o.idiArity
                     , idiIsOneShot = o.idiIsOneShot
                     , idiUnfolding = cvtUnfolding o.idiUnfolding
                     , idiInlinePragma = o.idiInlinePragma
                     , idiOccInfo = cvtOccInfo o.idiOccInfo
                     , idiStrictnessSig = o.idiStrictnessSig
                     , idiDemandSig = o.idiDemandSig
                     , idiCallArity = o.idiCallArity
                     }

cvtUnfolding :: O.Unfolding O.SBinder O.BinderId -> Unfolding
cvtUnfolding O.NoUnfolding = NoUnfolding
cvtUnfolding O.BootUnfolding = BootUnfolding
cvtUnfolding (O.OtherCon cons) = OtherCon (map cvtAltCon cons)
cvtUnfolding O.DFunUnfolding = DFunUnfolding
cvtUnfolding o@(O.CoreUnfolding {}) = CoreUnfolding { unfTemplate = cvtExpr o.unfTemplate
                                                    , unfIsValue = o.unfIsValue
                                                    , unfIsConLike = o.unfIsConLike
                                                    , unfIsWorkFree = o.unfIsWorkFree
                                                    , unfGuidance = o.unfGuidance
                                                    }


cvtOccInfo :: O.OccInfo -> OccInfo
cvtOccInfo O.OccManyOccs = OccManyOccs
cvtOccInfo O.OccDead = OccDead
cvtOccInfo O.OccOneOcc = OccOneOcc
cvtOccInfo (O.OccLoopBreaker x) = OccLoopBreaker x

cvtIdDetails :: O.IdDetails -> IdDetails
cvtIdDetails O.VanillaId = VanillaId
cvtIdDetails O.RecSelId = RecSelId
cvtIdDetails O.DataConWorkId = DataConWorkId
cvtIdDetails O.DataConWrapId = DataConWrapId
cvtIdDetails O.ClassOpId = ClassOpId
cvtIdDetails O.PrimOpId = PrimOpId
cvtIdDetails O.TickBoxOpId = TickBoxOpId
cvtIdDetails O.DFunId = DFunId
cvtIdDetails O.CoVarId = CoVarId
cvtIdDetails (O.JoinId x) = JoinId x

cvtLit :: O.Lit -> Lit
cvtLit (O.MachChar c) = MachChar c
cvtLit (O.MachStr s) = MachStr (T.decodeUtf8 s)
cvtLit O.MachNullAddr = MachNullAddr
cvtLit (O.MachInt x) = MachInt (showText x)
cvtLit (O.MachInt64 x) = MachInt64 (showText x)
cvtLit (O.MachWord x) = MachWord (showText x)
cvtLit (O.MachWord64 x) = MachWord64 (showText x)
cvtLit (O.MachFloat x) = MachFloat (showText x)
cvtLit (O.MachDouble x) = MachDouble (showText x)
cvtLit (O.MachLabel x) = MachLabel (showText x)
cvtLit (O.LitInteger x) = LitInteger (showText x)
cvtLit (O.LitNatural x) = LitInteger (showText x)
cvtLit O.LitRubbish = LitRubbish

cvtTyCon :: O.TyCon -> TyCon
cvtTyCon (O.TyCon t u) = TyCon t (cvtUnique u)

cvtType :: O.SType -> Type
cvtType (O.VarTy b) = VarTy (cvtBinderId b)
cvtType (O.FunTy t1 t2) = FunTy (cvtType t1) (cvtType t2)
cvtType (O.TyConApp con ts) = TyConApp (cvtTyCon con) (map cvtType ts)
cvtType (O.AppTy t1 t2) = AppTy (cvtType t1) (cvtType t2)
cvtType (O.ForAllTy b t) = ForAllTy (cvtBinder b) (cvtType t)
cvtType O.LitTy = LitTy
cvtType O.CoercionTy = CoercionTy

cvtModule :: Int -> O.SModule -> Module
cvtModule id o = Module { moduleName = cvtModuleName o.moduleName
                        , modulePhase = o.modulePhase
                        , modulePhaseId = id
                        , moduleTopBindings = map cvtTopBinding o.moduleTopBindings
                        }

cvtExpr :: O.SExpr -> Expr
cvtExpr (O.EVar b) = EVar (cvtBinderId b)
cvtExpr (O.EVarGlobal en) = EVarGlobal (cvtExternalName en)
cvtExpr (O.ELit l) = ELit (cvtLit l)
cvtExpr (O.EApp e1 e2) = EApp (cvtExpr e1) (cvtExpr e2)
cvtExpr (O.ETyLam b e) = ETyLam (cvtBinder b) (cvtExpr e)
cvtExpr (O.ELam b e) = ELam (cvtBinder b) (cvtExpr e)
cvtExpr (O.ELet bs e) = let go (b', e') = (cvtBinder b', cvtExpr e')
                       in ELet (map go bs) (cvtExpr e)
cvtExpr (O.ECase e b alts) = ECase (cvtExpr e) (cvtBinder b) (map cvtAlt alts)
cvtExpr (O.ETick t e) = ETick (cvtTick t) (cvtExpr e)
cvtExpr (O.EType t) = EType (cvtType t)
cvtExpr (O.ECoercion) = ECoercion

cvtAlt :: O.SAlt -> Alt
cvtAlt o = Alt { altCon = cvtAltCon o.altCon
               , altBinders = map cvtBinder o.altBinders
               , altRHS = cvtExpr o.altRHS
               }

cvtAltCon :: O.AltCon -> AltCon
cvtAltCon (O.AltDataCon t) = AltDataCon t
cvtAltCon (O.AltLit l) = AltLit (cvtLit l)
cvtAltCon O.AltDefault = AltDefault

cvtLineCol :: O.LineCol -> LineCol
cvtLineCol o = LineCol { row = o.row
                       , column = o.column
                       }

cvtSrcSpan :: O.SrcSpan -> SrcSpan
cvtSrcSpan o = SrcSpan { spanFile = o.spanFile
                       , spanStart = cvtLineCol o.spanStart
                       , spanEnd = cvtLineCol o.spanEnd
                       }

cvtTick :: O.Tick -> Tick
cvtTick (O.SourceNote x) = SourceNote (cvtSrcSpan x)

cvtCoreStats :: O.CoreStats -> CoreStats
cvtCoreStats O.CoreStats {..} = CoreStats {..}

cvtTopBinder :: O.SBinder -> O.CoreStats -> O.SExpr -> TopBinder
cvtTopBinder b s e = TopBinder (cvtBinder b) (cvtCoreStats s) (cvtExpr e)

cvtTopBinding :: O.STopBinding -> TopBinding
cvtTopBinding (O.NonRecTopBinding b s e) = NonRecTopBinding (cvtTopBinder b s e)
cvtTopBinding (O.RecTopBinding xs) = RecTopBinding (map (uncurry3 cvtTopBinder) xs)




