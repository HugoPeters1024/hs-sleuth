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

serveCapturesApi :: [Ast.Capture] -> Handler [Ast.Capture]
serveCapturesApi = pure

type CaptureAPI = "capture" :> Capture "slug" String :> Get '[JSON] Ast.Capture

serveCaptureApi :: String -> Handler Ast.Capture
serveCaptureApi slug = liftIO $ readFromFile (captureFile slug) 


type CoreAPI = "core" :> Capture "slug" String :> Capture "modname" String :> Capture "phaseId" Int :> Get '[JSON] Module

serveCoreApi :: String -> String -> Int -> Handler Module
serveCoreApi slug modname phase = liftIO $ readFromFile $ coreDumpFile slug modname phase


type API = CapturesAPI
      :<|> CaptureAPI
      :<|> CoreAPI


handler :: [Ast.Capture] -> Server API
handler captures = 
    (serveCapturesApi captures)
  :<|> serveCaptureApi
  :<|> serveCoreApi


addAllOriginsMiddleware :: Application -> Application
addAllOriginsMiddleware baseApp = \req responseFunc -> baseApp req (responseFunc . addOriginsAllowed)
    where addOriginsAllowed :: Response -> Response
          addOriginsAllowed = mapResponseHeaders $ (:) ("Access-Control-Allow-Origin", "*")

app :: IO Application
app = do
    captures <- collectCaptures
    putStrLn "Serving..."
    pure $ addAllOriginsMiddleware $ serve (Proxy @API) (handler captures)



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
