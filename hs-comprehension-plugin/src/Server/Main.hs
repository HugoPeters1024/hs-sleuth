{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import HsComprehension.Ast
import HsComprehension.Plugin (coreDumpBaseDir, coreDumpFile, projectMetaFile, readFromFile)

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
import Network.Wai.Handler.Warp (run)
import Network.Wai

import HsComprehension.Meta

type SessionAPI = "session" :> Get '[JSON] SessionMeta

sessionApi :: Proxy SessionAPI
sessionApi = Proxy

serveSessionApi :: SessionMeta -> Handler SessionMeta
serveSessionApi = return


type ProjectMetaAPI = "meta" :> Capture "slug" String :> Get '[JSON] ProjectMeta

serveProjectMetaApi :: String -> Handler ProjectMeta
serveProjectMetaApi slug = liftIO $ readFromFile (projectMetaFile slug) 


type CoreAPI = "core" :> Capture "slug" String :> Capture "modname" String :> Capture "phaseId" Int :> Get '[JSON] SModule

serveCoreApi :: String -> String -> Int -> Handler SModule
serveCoreApi slug modname phase = liftIO $ readFromFile $ coreDumpFile slug modname phase


type API = SessionAPI
      :<|> ProjectMetaAPI
      :<|> CoreAPI


handler :: SessionMeta -> Server API
handler sessionMeta 
     = serveSessionApi sessionMeta
  :<|> serveProjectMetaApi
  :<|> serveCoreApi


addAllOriginsMiddleware :: Application -> Application
addAllOriginsMiddleware baseApp = \req responseFunc -> baseApp req (responseFunc . addOriginsAllowed)
    where addOriginsAllowed :: Response -> Response
          addOriginsAllowed = mapResponseHeaders $ (:) ("Access-Control-Allow-Origin", "*")

app :: IO Application
app = do
    sessionMeta <- collectSessionMeta
    pure $ addAllOriginsMiddleware $ serve (Proxy @API) (handler sessionMeta)


main :: IO ()
main = run 8080 =<< app

collectSessionMeta :: IO SessionMeta
collectSessionMeta = do
    dirs <- listDirectory coreDumpBaseDir
         >>= filterM (pure . isPrefixOf "coredump-")
    slugs <- pure dirs
         >>= mapM (pure . fromJust . stripPrefix "coredump-")

    projects <- pure slugs
            >>= mapM (pure . projectMetaFile)
            >>= mapM (readFromFile @ProjectMeta)
    pure $ SessionMeta projects
