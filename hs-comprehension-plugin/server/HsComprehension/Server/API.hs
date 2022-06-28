{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module HsComprehension.Server.API where

import HsComprehension.Ast hiding (Capture)
import qualified HsComprehension.Ast as Ast
import HsComprehension.Plugin (coreDumpBaseDir, coreDumpFile, captureFile, readFromFile)

import HsComprehension.Server.ElmDeriving

import Data.Proxy
import Data.List
import Data.Maybe
import System.Directory
import Control.Monad
import Control.Monad.IO.Class

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T

import Servant.API
import Servant
import Network.Wai

type CapturesAPI = "captures" :> Get '[JSON] [Ast.Capture]

serveCapturesApi :: Handler [Ast.Capture]
serveCapturesApi = liftIO collectCaptures

type CaptureAPI = "capture" :> Capture "slug" String :> Get '[JSON] Ast.Capture

serveCaptureApi :: String -> Handler Ast.Capture
serveCaptureApi slug = liftIO $ readFromFile (captureFile slug) 


type ModuleAPI = "module" :> Capture "slug" String :> Capture "modname" String :> Get '[JSON] Module

serveModuleApi :: String -> String -> Handler Module
serveModuleApi slug modname = liftIO $ readFromFile $ coreDumpFile slug modname


type API = CapturesAPI
      :<|> CaptureAPI
      :<|> ModuleAPI


handler :: Server API
handler = 
    serveCapturesApi
  :<|> serveCaptureApi
  :<|> serveModuleApi


addAllOriginsMiddleware :: Application -> Application
addAllOriginsMiddleware baseApp = \req responseFunc -> baseApp req (responseFunc . addOriginsAllowed)
    where addOriginsAllowed :: Response -> Response
          addOriginsAllowed = mapResponseHeaders $ (:) ("Access-Control-Allow-Origin", "*")

app :: IO Application
app = do
    putStrLn "Serving..."
    pure $ addAllOriginsMiddleware $ serve (Proxy @API) handler



collectCaptures :: IO [Ast.Capture]
collectCaptures = do
    dirs <- listDirectory coreDumpBaseDir
         >>= filterM (pure . isPrefixOf "coredump-")
    slugs <- pure dirs
         >>= mapM (pure . fromJust . stripPrefix "coredump-")

    captures <- pure slugs
            >>= mapM (pure . captureFile)
            >>= mapM (readFromFile @Ast.Capture)
    pure $ captures
