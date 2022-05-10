{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Main where

import HsComprehension.Plugin (coreDumpBaseDir, coreDumpFile, projectMetaFile, readFromFile)
import HsComprehension.Ast
import HsComprehension.Meta

import Control.Monad
import Data.Maybe
import Data.Aeson as JSON
import Data.Aeson.Encode.Pretty (encodePretty)
import Codec.Serialise (Serialise)

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types

import System.Directory

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Text (Text)
import Data.List
import qualified Data.Text as T



collectSessionMeta :: IO SessionMeta
collectSessionMeta = do
    slugs <- listDirectory coreDumpBaseDir
         >>= filterM (pure . isPrefixOf "coredump-")
         >>= mapM (pure . fromJust . stripPrefix "coredump-")
    let ret = SessionMeta slugs
    print ret
    pure ret


main :: IO ()
main = collectSessionMeta >>= server

fetchCore :: Text -> Text -> Text -> IO (Maybe ByteString)
fetchCore pidString modString idString = do
    let mid :: Int = read (T.unpack idString)
    let pid :: String = T.unpack pidString
    let fname = coreDumpFile pid (T.unpack modString) mid
    mod <- readSModule fname
    pure (Just (JSON.encode mod))

fetchProjectMeta :: Text -> IO (Maybe ByteString)
fetchProjectMeta pidString = do
    let pid :: String = T.unpack pidString
    Just <$> resJsonFile @ProjectMeta (projectMetaFile pid)

resJsonFile :: forall a. (Serialise a, ToJSON a) => FilePath -> IO ByteString
resJsonFile fname = do
    obj <- readFromFile @a fname
    pure (encodePretty @a obj)



app :: SessionMeta -> Application
app session rec respond = do
    content <- case pathInfo rec of
        (pidString:modString:idString:[]) -> fetchCore pidString modString idString
        (pidString:"meta":[])             -> fetchProjectMeta pidString
        ("session":[])                    -> pure $ Just $ encodePretty session
        _                       -> pure Nothing

    let headers = [("Content-Type", "text/json"), ("Access-Control-Allow-Origin", "*")]
    case content of
      Just content -> respond (responseLBS ok200 headers content)
      Nothing -> respond (responseLBS notFound404 headers "{ \"err\": \"something went wrong\" }")


                



server :: SessionMeta -> IO ()
server session = do
    putStrLn "Running server at http://localhost:8080"
    run 8080 $ app session
