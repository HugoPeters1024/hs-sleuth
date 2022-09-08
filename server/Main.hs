{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import HsComprehension.Plugin (CaptureView(..))
import HsComprehension.Server.API (app)

import Network.Wai.Handler.Warp (run)
import System.Process (runCommand)
import Data.Data (Data, Typeable)
import System.Console.CmdArgs (def, (&=), help, opt, summary, cmdArgs)
import Control.Monad (when, void, (>=>))
import System.Directory (getCurrentDirectory, doesDirectoryExist, canonicalizePath)

data Opts = Opts
  { debug :: Bool
  , project_root :: FilePath
  , port :: Int
  }
  deriving (Show, Data, Typeable)

parseArgs = Opts
  { debug = def &= help "Run in debug modus (Using elm reactor)"
  , project_root = "./" &= help "The project root whose captures to inspect"
  , port = 8080 &= help "What port to serve on"
  } 
  &= summary "HsComprehension backend server"


finalizeArgs :: Opts -> IO Opts
finalizeArgs = let 
  setDebugRoot opts = 
        if (debug opts && project_root opts == "./") then do 
          let project_root = "/home/hugo/repos/hs-comprehension/test-project/"
          putStrLn $ "DEBUG mode enabled, defaulting project_root to " <> project_root
          pure $ opts { project_root = project_root }
        else pure opts

  makePathAbsolute :: Opts -> IO Opts
  makePathAbsolute opts = do
    exists <- doesDirectoryExist (project_root opts)
    if exists
    then do 
      absolute <- canonicalizePath (project_root opts)
      pure $ opts { project_root = absolute }
    else do
      error $ "project-root (" <> project_root opts <> ") does not exist"

  in setDebugRoot >=> makePathAbsolute

  

main :: IO ()
main = do
  args <- cmdArgs parseArgs >>= finalizeArgs

  putStrLn $ "Serving captures from: " <> project_root args
  getCurrentDirectory >>= \d -> putStrLn ("Serving frontend from: " <> d)

  when (debug args) $ do
    putStrLn "DEBUG mode enabled, starting live elm reactor at port 8000"
    void $ runCommand "cd /home/hugo/repos/hs-comprehension/frontend/src && elm reactor"

  let view = CaptureView { cv_project_root = project_root args }
  putStrLn $ "Starting the app at http://localhost:" <> show (port args) <> "/"
  run (port args) (app view)

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM p t = p >>= \p' -> if p' then t else pure ()

ifM :: (Monad m) => m Bool -> m a -> m a -> m a
ifM p t f = p >>= \p' -> if p' then t else f
