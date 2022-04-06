{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Server (server) where

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types

import System.IO (openFile, IOMode(..))

import Data.Maybe

import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Read as T

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B

import Data.Aeson.Encode.Pretty (encodePretty)

import CoreLang.Types 

app :: [PassInfo] -> Application
app passes rec respond = do
    let fetchCore :: Text -> IO ByteString
        fetchCore idString = 
            let idx :: Int = fromMaybe 1 $ T.readMaybe $ T.unpack $ idString
            in pure $ encodePretty $ passes !! (idx-1)


        fetchSource :: Text -> IO ByteString
        fetchSource modName = do
            let filename = "app/Main.hs"
            handle <- openFile filename ReadMode
            B.hGetContents handle

    content <- case pathInfo rec of
          [] -> pure "no index"
          ("core":idString:_) -> fetchCore idString
          ("source":srcName:_) -> fetchSource srcName
          p           -> pure "unknown path"


    respond (responseLBS ok200 [("Content-Type", "text/json"), ("Access-Control-Allow-Origin", "*")] content)

server :: [PassInfo] -> IO ()
server passes = do
    putStrLn "Running server at http://localhost:8080"
    run 8080 (app passes)

