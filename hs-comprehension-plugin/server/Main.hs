module Main where

import HsComprehension.Server.API (app)
import Network.Wai.Handler.Warp (run)
import System.Process (runCommand)

main :: IO ()
main = do
  -- TODO: Just for dev
  runCommand "cd /home/hugo/repos/hs-comprehension/frontend/src && elm reactor"
  run 8080 =<< app
