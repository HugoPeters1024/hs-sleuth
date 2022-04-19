{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import HsComprehension.Plugin (coreDumpFile)
import HsComprehension.Ast



import Data.Maybe

import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty (encodePretty)

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import qualified Data.Text as T


main :: IO ()
main = server

fetchCore :: Text -> Text -> IO (Maybe ByteString)
fetchCore modString idString = do
    let id :: Int = read (T.unpack idString)
    let fname = coreDumpFile (T.unpack modString) id
    mod <- readSModule fname
    pure (Just (JSON.encode mod))


app :: Application
app rec respond = do
    content <- case pathInfo rec of
        (modString:idString:[]) -> fetchCore modString idString
        _                       -> pure Nothing

    let headers = [("Content-Type", "text/json"), ("Access-Control-Allow-Origin", "*")]
    case content of
      Just content -> respond (responseLBS ok200 headers content)
      Nothing -> respond (responseLBS notFound404 headers "{ \"err\": \"something went wrong\" }")


                



server :: IO ()
server = do
    putStrLn "Running server at http://localhost:8080"
    run 8080 app
