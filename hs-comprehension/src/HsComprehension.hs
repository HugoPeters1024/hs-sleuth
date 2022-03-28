{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DataKinds #-}
module HsComprehension (plugin, CoreTrace(..)) where

import Prelude hiding ((<>))
import Data.List
import Data.Maybe
import Data.IORef
import Control.Monad

import GHC
import GHC.Plugins
import Data.Data

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types

import Generation
import PrettyPrinting
import CoreCollection

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


pass :: Int -> SDoc -> IORef [PassInfo] -> ModGuts -> CoreM ModGuts
pass idx prevName ref guts = do
    dflags <- getDynFlags
    let sPrevName = showSDoc dflags prevName
    let binds = getAllTopLevelDefs (mg_binds guts)
    let body = concatMap ((++"\n") . showBind dflags) (map (uncurry NonRec) binds)
    let passInfo = PassInfo { idx = idx
                            , title = sPrevName
                            , ast = body
                            , raw = map (uncurry NonRec) binds
                            }
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

annPassViews :: [PassView] -> [PassView]
annPassViews views = zipWith3 go (annPred views) (annSucc views) views
    where go prev next PassView {..} = PassView { prevPass = prev, nextPass = next, .. }

printInfoPass :: IORef [PassInfo] -> CoreToDo
printInfoPass ref = CoreDoPluginPass "Print Info" $ \guts -> do
    collection <- liftIO $ reverse <$> readIORef ref
    views <- liftIO $ mapM infoToView collection

    -- annotate with previous and next pass if available
    let views' = annPassViews views
    let globalInfo = collectInfo views'

    liftIO $ outputCss globalInfo

    forM_ views' $ \view -> do 
        putMsgS $ "dumping output after " ++ view.info.title
        liftIO $ renderPass globalInfo view >>= saveToFile view.filepath


    liftIO $ generateElm @'[CL.CoreLiteral, CL.CoreTerm, CL.CoreBind, CL.CoreBndr, CL.CoreAltCon, CL.CoreAlt] $ defaultSettings "." ["Core", "Generated"]

    test <- mapM CL.coreLangBind ((collection !! 0).raw)
    liftIO (server test)

    pure guts

app :: [CL.CoreBind] -> Application
app term _ respond = respond (responseLBS ok200 [("Content-Type", "text/plain"), ("Access-Control-Allow-Origin", "*")] (encodePretty term))

server :: [CL.CoreBind] -> IO ()
server term = do
    putStrLn "Running server at http://localhost:8080"
    run 8080 (app term)
