{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DisambiguateRecordFields #-}
module Generation where

import Data.String (IsString(..))
import Control.Monad (forM_)
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import System.IO.Temp
import System.IO
import qualified Data.ByteString as BS

import GHC.Plugins
import System.Process

import qualified Data.String.Interpolate as I (i)
import PrettyPrinting
import qualified CoreCollection as CC

data PassView  = PassView { title :: String
                          , idx :: Int
                          , codeblock :: String
                          , prevPass :: Maybe PassView
                          , nextPass :: Maybe PassView
                          , filepath :: FilePath
                          }

data GlobalPassInfo = GlobalPassInfo { nrViews :: Int
                                     }

collectInfo :: [PassView] -> GlobalPassInfo
collectInfo views = GlobalPassInfo { nrViews = length views
                                   }


infoToView :: CC.PassInfo -> IO PassView
infoToView info = do
    path <- emptySystemTempFile $ [I.i|hs-comprehension.html|]
    pure $ PassView { title = info.title
                    , idx = info.idx
                    , codeblock = info.ast
                    , prevPass = Nothing
                    , nextPass = Nothing
                    , filepath = path
                    }


codeBlock :: String -> Html
codeBlock code = H.pre ! A.class_ "code"  $ H.unsafeByteString [I.i| #{code}|]

maybeHtml :: Maybe Html -> Html
maybeHtml Nothing = pure ()
maybeHtml (Just html) = html

buttonToPass :: PassView -> Html
buttonToPass view = H.a ! A.href [I.i|#{filepath view}|] $ do
                        H.button (toHtml view.title)


renderPass :: GlobalPassInfo -> PassView -> IO Html
renderPass globals view = do
    guts_colored <- highlight view.codeblock
    pure $ docTypeHtml $ do
        H.head $ do
            H.title $ H.string view.title
            H.style $ H.unsafeByteString pygmentCss
        H.body $ do
            let title = view.title
                idx = view.idx
                total = globals.nrViews
            H.h1 $ [I.i|#{title} #{idx}/#{total}|]
            H.hr
            codeBlock guts_colored
            maybeHtml $ buttonToPass <$> view.prevPass
            H.br
            maybeHtml $ buttonToPass <$> view.nextPass
        
saveToFile :: FilePath -> Html -> IO ()
saveToFile path html = 
    let 
        bs :: BS.ByteString
        bs = BS.toStrict $ renderHtml html 
    in 
        withFile path WriteMode $ \handle -> do
            BS.hPutStr handle bs
            putStrLn $ "file://" ++ path

highlight :: String -> IO String
highlight = readProcess "/home/hugo/repos/hs-comprehension/hs-comprehension/scripts/hightlight.py" []


pygmentCss :: BS.ByteString
pygmentCss = [I.i| 
    .hll { background-color: #49483e }
    .c { color: #95917e } /* Comment */
    .err { color: #960050; background-color: #1e0010 } /* Error */
    .k { color: #66d9ef } /* Keyword */
    .l { color: #ae81ff } /* Literal */
    .n { color: #f8f8f2 } /* Name */
    .o { color: #f92672 } /* Operator */
    .p { color: #f8f8f2 } /* Punctuation */
    .ch { color: #75715e } /* Comment.Hashbang */
    .cm { color: #75715e } /* Comment.Multiline */
    .cp { color: #75715e } /* Comment.Preproc */
    .cpf { color: #75715e } /* Comment.PreprocFile */
    .c1 { color: #95917e } /* Comment.Single */
    .cs { color: #75715e } /* Comment.Special */
    .gd { color: #f92672 } /* Generic.Deleted */
    .ge { font-style: italic } /* Generic.Emph */
    .gi { color: #a6e22e } /* Generic.Inserted */
    .gs { font-weight: bold } /* Generic.Strong */
    .gu { color: #75715e } /* Generic.Subheading */
    .kc { color: #66d9ef } /* Keyword.Constant */
    .kd { color: #66d9ef } /* Keyword.Declaration */
    .kn { color: #f92672 } /* Keyword.Namespace */
    .kp { color: #66d9ef } /* Keyword.Pseudo */
    .kr { color: #66d9ef } /* Keyword.Reserved */
    .kt { color: #66d9ef } /* Keyword.Type */
    .ld { color: #e6db74 } /* Literal.Date */
    .m { color: #ae81ff } /* Literal.Number */
    .s { color: #e6db74 } /* Literal.String */
    .na { color: #a6e22e } /* Name.Attribute */
    .nb { color: #f8f8f2 } /* Name.Builtin */
    .nc { color: #a6e22e } /* Name.Class */
    .no { color: #66d9ef } /* Name.Constant */
    .nd { color: #a6e22e } /* Name.Decorator */
    .ni { color: #f8f8f2 } /* Name.Entity */
    .ne { color: #a6e22e } /* Name.Exception */
    .nf { color: #a6e22e } /* Name.Function */
    .nl { color: #f8f8f2 } /* Name.Label */
    .nn { color: #f8f8f2 } /* Name.Namespace */
    .nx { color: #a6e22e } /* Name.Other */
    .py { color: #f8f8f2 } /* Name.Property */
    .nt { color: #f92672 } /* Name.Tag */
    .nv { color: #f8f8f2 } /* Name.Variable */
    .ow { color: #f92672 } /* Operator.Word */
    .w { color: #f8f8f2 } /* Text.Whitespace */
    .mb { color: #ae81ff } /* Literal.Number.Bin */
    .mf { color: #ae81ff } /* Literal.Number.Float */
    .mh { color: #ae81ff } /* Literal.Number.Hex */
    .mi { color: #ae81ff } /* Literal.Number.Integer */
    .mo { color: #ae81ff } /* Literal.Number.Oct */
    .sa { color: #e6db74 } /* Literal.String.Affix */
    .sb { color: #e6db74 } /* Literal.String.Backtick */
    .dl { color: #e6db74 } /* Literal.String.Delimiter */
    .sd { color: #e6db74 } /* Literal.String.Doc */
    .s2 { color: #e6db74 } /* Literal.String.Double */
    .se { color: #ae81ff } /* Literal.String.Escape */
    .sh { color: #e6db74 } /* Literal.String.Heredoc */
    .si { color: #e6db74 } /* Literal.String.Interpol */
    .sx { color: #e6db74 } /* Literal.String.Other */
    .sr { color: #e6db74 } /* Literal.String.Regex */
    .s1 { color: #e6db74 } /* Literal.String.Single */
    .ss { color: #e6db74 } /* Literal.String.Symbol */
    .bp { color: #f8f8f2 } /* Name.Builtin.Pseudo */
    .fm { color: #a6e22e } /* Name.Function.Magic */
    .vc { color: #f8f8f2 } /* Name.Variable.Class */
    .vg { color: #f8f8f2 } /* Name.Variable.Global */
    .vi { color: #f8f8f2 } /* Name.Variable.Instance */
    .vm { color: #f8f8f2 } /* Name.Variable.Magic */
    .il { color: #ae81ff } /* Literal.Number.Integer.Long */
    pre.code { background-color: #173E46; padding: 2em; }
    |]
