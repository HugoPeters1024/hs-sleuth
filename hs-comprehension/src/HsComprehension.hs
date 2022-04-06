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

import GHC
import GHC.Plugins
import Data.Data


import qualified Data.Text as T

import Uniqify
import SinkSpan
import Server

import qualified CoreLang.Types as CL
import qualified CoreLang.Cvt as CL

import Elm (Elm, ElmStreet(..), elmStreetParseJson, elmStreetToJson, generateElm, defaultSettings)


plugin :: Plugin
plugin = defaultPlugin
  { installCoreToDos = install
  , pluginRecompile = purePlugin
  }

data PlugState = PlugState 
    { passes :: [CL.PassInfo]
    }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
    let plugState = PlugState []
    ref <- liftIO $ newIORef plugState
    let mkPass = \idx prevName -> CoreDoPluginPass "Collection Pass" (pass idx prevName ref)
    let firstPass = mkPass 1 "Desugared" 

    let passes = concat $ zipWith3 (\idx x y -> [x, y idx (ppr x)]) [2..] todo (repeat mkPass)
    pure (firstPass : passes ++ [printInfoPass ref])

pass :: Int -> SDoc -> IORef PlugState -> ModGuts -> CoreM ModGuts
pass idx prevName ref guts = do
    dflags <- getDynFlags
    liftIO $ putStrLn $ showSDoc dflags (ppr (mg_module guts))
    let title = T.pack $ showSDoc dflags prevName

    uniqified <- liftIO $ (runUnique $ uniqProgram (mg_binds guts) >>= pure . map sinkSpan)

    cvtBinds <- CL.cvtCoreLang $ getAllTopLevelDefs uniqified

    let passInfo = CL.PassInfo { idx = idx
                               , title = title
                               , binds = cvtBinds
                               }

    liftIO $ modifyIORef ref (\s -> 
        s { passes = passInfo:s.passes
          })

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
    passes <- liftIO $ readIORef ref >>= \s -> pure $ reverse s.passes

    liftIO $ generateElm @'[CL.CoreId, CL.PassInfo, CL.CoreLiteral, CL.CoreTerm, CL.CoreBind, CL.CoreAltCon, CL.CoreAlt] $ defaultSettings "." ["Core", "Generated"]

    liftIO (server passes)

    pure guts



