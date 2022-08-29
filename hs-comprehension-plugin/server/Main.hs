{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import HsComprehension.Plugin (CaptureView(..))
import HsComprehension.Server.API (app)

import Network.Wai.Handler.Warp (run)
import System.Process (runCommand)
import Data.Data (Data, Typeable)
import System.Console.CmdArgs (def, (&=), help, opt, summary, cmdArgs)
import Control.Monad (when, void)
import System.Directory (getCurrentDirectory)

data Opts = Opts
  { debug :: Bool
  , project_root :: FilePath
  }
  deriving (Show, Data, Typeable)

parseArgs = Opts
  { debug = def &= help "Run in debug modus (Using elm reactor)"
  , project_root = "./" &= help "The project root whose captures to inspect"
  } 
  &= summary "HsComprehension backend server"


finalizeArgs :: Opts -> IO Opts
finalizeArgs = 
  let setDebugRoot opts = 
        if (debug opts) then do 
          let project_root = "/home/hugo/repos/hs-comprehension/test-project/"
          putStrLn $ "DEBUG mode enabled, defaulting project_root to " <> project_root
          pure $ opts { project_root = project_root }
        else pure opts

  in setDebugRoot

  

main :: IO ()
main = do
  args <- cmdArgs parseArgs >>= finalizeArgs

  getCurrentDirectory >>= \d -> putStrLn ("Serving frontend from: " <> d)

  when (debug args) $ do
    putStrLn "DEBUG mode enabled, starting live elm reactor at port 8000"
    void $ runCommand "cd /home/hugo/repos/hs-comprehension/frontend/src && elm reactor"

  let view = CaptureView { cv_project_root = project_root args }
  run 8080 =<< (app view)
