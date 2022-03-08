{-# LANGUAGE OverloadedStrings #-}
module Generation where

import Control.Monad (forM_)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import System.IO.Temp
import System.IO
import qualified Data.ByteString.Lazy as BS

pageTest :: Html
pageTest = docTypeHtml $ do
    H.head $ do
        H.title "test"
    H.body $ do
        p "beep boop lol"


saveToFile :: Html -> IO ()
saveToFile html = 
    let 
        bs = renderHtml html 
    in 
        do
            path <- emptySystemTempFile "hs-comprehension.html"
            withFile path WriteMode $ \handle -> do
                BS.hPutStr handle bs
                putStrLn $ "file://" ++ path



