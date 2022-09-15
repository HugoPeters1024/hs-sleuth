{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module HsComprehension.Server.API where

import HsComprehension.Ast hiding (Capture)
import qualified HsComprehension.Ast as Ast
import HsComprehension.Plugin (coreDumpBaseDir, coreDumpDir, coreDumpFile, captureFile, readFromFile, CaptureView(..))

import HsComprehension.Server.ElmDeriving

import Data.Proxy
import Data.List
import Data.Maybe
import System.Directory as FP
import System.FilePath.Posix as FP
import Control.Monad
import Control.Monad.IO.Class

import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BL

import Network.HTTP.Media ((//), (/:))
import Servant.API
import Servant
import Network.Wai
import Network.Wai.Middleware.Gzip (gzip, def)

data HTML = HTML

newtype RawHtml = RawHtml { unRaw :: BL.ByteString }

instance Accept HTML where
  contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML RawHtml where
  mimeRender = const unRaw

type SettingsAPI = "settings" :> Get '[JSON] Ast.ServerSettings

serveSettingsAPI :: CaptureView -> Handler ServerSettings 
serveSettingsAPI view = pure $ ServerSettings { st_baseDir = T.pack (coreDumpBaseDir view) }

type CapturesAPI = "captures" :> Get '[JSON] [Ast.Capture]

serveCapturesApi :: CaptureView -> Handler [Ast.Capture]
serveCapturesApi = liftIO . collectCaptures

type CaptureAPI = "capture" :> Capture "slug" String :> Get '[JSON] Ast.Capture

serveCaptureApi :: CaptureView -> String -> Handler Ast.Capture
serveCaptureApi view slug = liftIO $ readFromFile (captureFile view slug) 

type DeleteCaptureAPI = "capture_delete" :> Capture "slug" String :> Get '[JSON] ()

serveDeleteCaptureAPI :: CaptureView -> String -> Handler ()
serveDeleteCaptureAPI view slug = liftIO $ do
    let dir = coreDumpDir view slug
    putStrLn ("deleting " <> dir)
    removeDirectoryRecursive dir

type SrcAPI = "src" :> Capture "slug" String :> Capture "modname" String :> Get '[HTML] RawHtml

serveSrcApi :: CaptureView -> String -> String -> Handler RawHtml
serveSrcApi view slug mod = liftIO $ RawHtml <$> BL.readFile (coreDumpDir view slug `FP.combine` mod ++ ".hs")



type ModuleAPI = "module" :> Capture "slug" String :> Capture "modname" String :> Get '[JSON] Module

serveModuleApi :: CaptureView -> String -> String -> Handler Module
serveModuleApi view slug modname = liftIO $ readFromFile $ coreDumpFile view slug modname


type IndexAPI = Get '[HTML] RawHtml

serveIndexApi :: Handler RawHtml
serveIndexApi = liftIO $ RawHtml <$> BL.readFile "static/index.html"


type API = SettingsAPI
      :<|> CapturesAPI
      :<|> CaptureAPI
      :<|> DeleteCaptureAPI
      :<|> SrcAPI
      :<|> ModuleAPI
      :<|> IndexAPI
      :<|> Raw



handler :: CaptureView -> Server API
handler view = 
    (serveSettingsAPI view)
  :<|> (serveCapturesApi view)
  :<|> (serveCaptureApi view)
  :<|> (serveDeleteCaptureAPI view)
  :<|> (serveSrcApi view)
  :<|> (serveModuleApi view)
  :<|> serveIndexApi
  :<|> serveDirectoryWebApp "./static"


addAllOriginsMiddleware :: Application -> Application
addAllOriginsMiddleware baseApp = \req responseFunc -> baseApp req (responseFunc . addOriginsAllowed)
    where addOriginsAllowed :: Response -> Response
          addOriginsAllowed = mapResponseHeaders $ (("Access-Control-Allow-Origin", "*"):)

app :: CaptureView -> Application
app view = addAllOriginsMiddleware $ serve (Proxy @API) (handler view)
--gzip def $ addAllOriginsMiddleware $ serve (Proxy @API) (handler view)


listDirectorySafe :: FilePath -> IO [FilePath]
listDirectorySafe path = do
  exists <- doesDirectoryExist path
  if exists then listDirectory path else pure []


collectCaptures :: CaptureView -> IO [Ast.Capture]
collectCaptures view = do
    dirs <- listDirectorySafe (coreDumpBaseDir view)
         >>= filterM (pure . isPrefixOf "coredump-")
    slugs <- pure dirs
         >>= mapM (pure . fromJust . stripPrefix "coredump-")

    captures <- pure slugs
            >>= mapM (pure . captureFile view)
            >>= mapM (readFromFile @Ast.Capture)
    pure $ captures
