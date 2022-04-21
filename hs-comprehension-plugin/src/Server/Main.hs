{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Main where

import HsComprehension.Plugin (coreDumpFile, projectMetaFile, readFromFile)
import HsComprehension.Ast
import HsComprehension.Meta

import Data.Maybe

import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty (encodePretty)
import Codec.Serialise (Serialise)

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

fetchProjectMeta :: IO (Maybe ByteString)
fetchProjectMeta = Just <$> resJsonFile @ProjectMeta projectMetaFile

resJsonFile :: forall a. (Serialise a, ToJSON a) => FilePath -> IO ByteString
resJsonFile fname = do
    obj <- readFromFile @a fname
    pure (encodePretty @a obj)



app :: Application
app rec respond = do
    content <- case pathInfo rec of
        (modString:idString:[]) -> fetchCore modString idString
        ("meta":[])             -> fetchProjectMeta
        _                       -> pure Nothing

    let headers = [("Content-Type", "text/json"), ("Access-Control-Allow-Origin", "*")]
    case content of
      Just content -> respond (responseLBS ok200 headers content)
      Nothing -> respond (responseLBS notFound404 headers "{ \"err\": \"something went wrong\" }")


                



server :: IO ()
server = do
    putStrLn "Running server at http://localhost:8080"
    run 8080 app
