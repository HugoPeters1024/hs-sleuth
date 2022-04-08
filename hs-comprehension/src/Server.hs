{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Server (server) where

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types

import System.IO (openFile, IOMode(..))

import Data.Maybe

import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Read as T

import qualified Data.Map as M

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B

import Data.Aeson.Encode.Pretty (encodePretty)

import CoreLang.Types 
import PlugState

app :: PlugState -> Application
app state rec respond = do
    let fetchCore :: Text -> IO ByteString
        fetchCore modString = 
            let modName = T.unpack modString
                modInfo = M.lookup modName state
             in case modInfo of
                  Nothing -> pure $ "Module not found"
                  Just modInfo -> pure $ encodePretty $ modInfo


        fetchSource :: Text -> IO ByteString
        fetchSource modName = do
            let filename = "app/" ++ T.unpack modName ++ ".hs"
            handle <- openFile filename ReadMode
            B.hGetContents handle

        dumpState :: IO ByteString
        dumpState = pure $ encodePretty state

        fetchMeta :: IO ByteString
        fetchMeta = let
            meta = MetaInfo { modules = map T.pack $ M.keys state
                            }
            in pure $ encodePretty meta

    content <- case pathInfo rec of
          [] -> pure "no index"
          ("dump":_) -> dumpState
          ("core":modString:_) -> fetchCore modString
          ("source":srcName:_) -> fetchSource srcName
          ("meta":_) -> fetchMeta
          p           -> pure "unknown path"


    respond (responseLBS ok200 [("Content-Type", "text/plain"), ("Access-Control-Allow-Origin", "*")] content)

server :: PlugState -> IO ()
server pages = do
    putStrLn "Running server at http://localhost:8080"
    run 8080 (app pages)

