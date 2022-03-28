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

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types

import qualified Data.Text as T
import Text.Read

import qualified CoreLang as CL
import Data.Aeson.Encode.Pretty (encodePretty)

import Elm (Elm, ElmStreet(..), elmStreetParseJson, elmStreetToJson, generateElm, defaultSettings)


plugin :: Plugin
plugin = defaultPlugin
  { installCoreToDos = install
  , pluginRecompile = purePlugin
  }

install :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
install _ todo = do
    ref <- liftIO $ newIORef []
    let mkPass = \idx prevName -> CoreDoPluginPass "Collection Pass" (pass idx prevName ref)
    let firstPass = mkPass 1 "Desugared" 

    let passes = concat $ zipWith3 (\idx x y -> [x, y idx (ppr x)]) [2..] todo (repeat mkPass)
    pure (firstPass : passes ++ [printInfoPass ref])


pass :: Int -> SDoc -> IORef [CL.PassInfo] -> ModGuts -> CoreM ModGuts
pass idx prevName ref guts = do
    dflags <- getDynFlags
    let title = T.pack $ showSDoc dflags prevName
    binds <- mapM CL.coreLangBind $ getAllTopLevelDefs (mg_binds guts)

    let passInfo = CL.PassInfo {..}
    liftIO $ modifyIORef ref (passInfo:)
    pure guts

annPred :: [a] -> [Maybe a]
annPred [] = []
annPred [x] = [Nothing]
annPred (x:y:xs) = Nothing:Just x:tail (annPred (y:xs))

annSucc :: [a] -> [Maybe a]
annSucc [] = []
annSucc [x] = [Nothing]
annSucc (x:y:xs) = Just y:annSucc (y:xs)

printInfoPass :: IORef [CL.PassInfo] -> CoreToDo
printInfoPass ref = CoreDoPluginPass "Print Info" $ \guts -> do
    passes <- liftIO $ reverse <$> readIORef ref

    liftIO $ generateElm @'[CL.PassInfo, CL.CoreLiteral, CL.CoreTerm, CL.CoreBind, CL.CoreBndr, CL.CoreAltCon, CL.CoreAlt] $ defaultSettings "." ["Core", "Generated"]

    liftIO (server passes)

    pure guts

app :: [CL.PassInfo] -> Application
app passes rec respond = let 
    idx :: Int = fromMaybe 1 $ listToMaybe (pathInfo rec) >>= readMaybe . T.unpack
    in respond (responseLBS ok200 [("Content-Type", "text/plain"), ("Access-Control-Allow-Origin", "*")] (encodePretty (passes !! (idx-1))))

server :: [CL.PassInfo] -> IO ()
server passes = do
    putStrLn "Running server at http://localhost:8080"
    run 8080 (app passes)
