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
import qualified CoreLang.Utils as CL

import System.IO.Unsafe
import Elm (Elm, ElmStreet(..), elmStreetParseJson, elmStreetToJson, generateElm, defaultSettings)


plugin :: Plugin
plugin = defaultPlugin
  { installCoreToDos = install
--  , pluginRecompile = purePlugin
  }

{-# NOINLINE ref #-}
ref :: IORef PlugState
ref = unsafePerformIO $ newIORef $ M.empty


install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
    modName <- (showSDocUnsafe . ppr) <$> getModule
    liftIO $ do
        putStrLn $ "Installing plugin for " ++ modName
        let moduleInfo = CL.ModuleInfo { nrpasses = 0
                                       , srcbindings = []
                                       , name = T.pack modName
                                       }
        modifyIORef ref $ M.insert modName (moduleInfo, [])

    let mkPass = \first idx prevName -> CoreDoPluginPass "Collection Pass" (pass first idx prevName ref)
    let firstPass = mkPass True 1 "Desugared" 

    let passes = concat $ zipWith3 (\idx x y -> [x, y idx (ppr x)]) [2..] todo (repeat (mkPass False))
    pure (firstPass : passes ++ [printInfoPass ref])

pass :: Bool -> Int -> SDoc -> IORef PlugState -> ModGuts -> CoreM ModGuts
pass first idx prevName ref guts = do
    let title = T.pack $ showSDocUnsafe prevName

    uniqified <- liftIO $ pure (mg_binds guts) -- runUnique $ uniqProgram (mg_binds guts)
    cvtBinds <- CL.cvtCoreLang $ getAllTopLevelDefs uniqified

    -- The first pass is the desugar pass, any binding that is not prefixed with a dollar
    -- symbol are a decent approximation for the existence of a direct counterpart in the source
    when first $ liftIO $ do
        let filterFunc :: CL.CoreId -> Bool
            filterFunc var = not $ T.isPrefixOf "$" var.name

            srcbindings = map (\x -> x.unique) $ filter filterFunc (map CL.coreLangBindBndr cvtBinds)
        modifyIORef ref $ setSrcBindings (modName guts) srcbindings

    let moduleName = showSDocUnsafe (ppr (mg_module guts))
    let passInfo = CL.PassInfo { idx = idx
                               , title = title
                               , binds = cvtBinds
                               , modname = T.pack moduleName
                               }

    liftIO $ modifyIORef ref $ addPass moduleName passInfo
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
        putStrLn $ "finalizing info for module " ++ modName guts

        when (modName guts == "Main") $ do
            generateElm @'[CL.MetaInfo, CL.ModuleInfo, CL.CoreId, CL.PassInfo, CL.CoreLiteral, CL.CoreTerm, CL.CoreBind, CL.CoreAltCon, CL.CoreAlt] $ defaultSettings "." ["Core", "Generated"]
            server =<< readIORef ref

    pure guts



