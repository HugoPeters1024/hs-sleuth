module Main where

import HsComprehension.Server.API (app)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = run 8080 =<< app
