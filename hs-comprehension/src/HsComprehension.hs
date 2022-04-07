{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}
module HsComprehension (plugin) where

import Prelude hiding ((<>))
import Data.List
import Data.Maybe
import Data.IORef

import Control.Monad

import CoreCollection
import PlugState
import Uniqify
import SinkSpan
import Server

import GHC
import GHC.Plugins
import Data.Data

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as M

import qualified CoreLang.Types as CL
import qualified CoreLang.Cvt as CL

import System.IO.Unsafe
import Elm (Elm, ElmStreet(..), elmStreetParseJson, elmStreetToJson, generateElm, defaultSettings)


plugin :: Plugin
plugin = defaultPlugin
  { installCoreToDos = install
  , pluginRecompile = purePlugin
  }

ref :: IORef PlugState
ref = unsafePerformIO $ newIORef $ PlugState M.empty


install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
    let plugState = PlugState M.empty
    let mkPass = \idx prevName -> CoreDoPluginPass "Collection Pass" (pass idx prevName ref)
    let firstPass = mkPass 1 "Desugared" 

    let passes = concat $ zipWith3 (\idx x y -> [x, y idx (ppr x)]) [2..] todo (repeat mkPass)
    pure (firstPass : passes ++ [printInfoPass ref])

pass :: Int -> SDoc -> IORef PlugState -> ModGuts -> CoreM ModGuts
pass idx prevName ref guts = do
    dflags <- getDynFlags
    let title = T.pack $ showSDoc dflags prevName

    uniqified <- liftIO $ runUnique $ uniqProgram (mg_binds guts)

    cvtBinds <- CL.cvtCoreLang $ getAllTopLevelDefs uniqified

    let moduleName = showSDocUnsafe (ppr (mg_module guts))
    let passInfo = CL.PassInfo { idx = idx
                               , title = title
                               , binds = cvtBinds
                               , totalpasses = -1
                               , modname = T.pack moduleName
                               }

    liftIO $ modifyIORef ref (\s -> s { modulePasses = M.insertWith (++) moduleName [passInfo] s.modulePasses })

    pure guts { mg_binds = uniqified }

annPred :: [a] -> [Maybe a]
annPred [] = []
annPred [x] = [Nothing]
annPred (x:y:xs) = Nothing:Just x:tail (annPred (y:xs))

annSucc :: [a] -> [Maybe a]
annSucc [] = []
annSucc [x] = [Nothing]
annSucc (x:y:xs) = Just y:annSucc (y:xs)


printInfoPass :: IORef PlugState -> CoreToDo
printInfoPass ref = CoreDoPluginPass "Print Info" $ \guts -> do

    liftIO $ do
        state <- readIORef ref
        let embellishPass :: Int -> CL.PassInfo -> CL.PassInfo
            embellishPass length pass = pass { CL.totalpasses = length }

        passes <- case M.lookup (modName guts) state.modulePasses of
                       Nothing -> error "Module was not collected.."
                       Just passes -> pure $ map (embellishPass (length passes)) (reverse passes)

        
        writeIORef ref $ state { modulePasses = M.insert (modName guts) passes state.modulePasses }

    when (modName guts == "Main") $ do
        liftIO $ generateElm @'[CL.MetaInfo, CL.CoreId, CL.PassInfo, CL.CoreLiteral, CL.CoreTerm, CL.CoreBind, CL.CoreAltCon, CL.CoreAlt] $ defaultSettings "." ["Core", "Generated"]
        liftIO $ server =<< readIORef ref

    pure guts



