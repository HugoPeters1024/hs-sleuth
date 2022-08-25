module Main where

import HsComprehension.Server.API (app, Env(..))
import Network.Wai.Handler.Warp (run)
import System.Process (runCommand)

main :: IO ()
main = do
  -- TODO: Just for dev
  putStrLn "starting elm backend.."
  runCommand "cd /home/hugo/repos/hs-comprehension/frontend/src && elm reactor"

  let env = Env { project_root = "/home/hugo/repos/hs-comprehension/test-project" }
  run 8080 =<< (app env)
