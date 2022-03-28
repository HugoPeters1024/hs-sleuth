{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
module Generation where

import Control.Monad (forM_)
import System.IO.Temp
import System.IO

import GHC.Plugins
import GHC.Records
import System.Process
import System.FilePath

import qualified Data.String.Interpolate as I (i)
import Data.Maybe

import IHP.HSX.QQ
import IHP.HSX.ConvertibleStrings ()
import IHP.HSX.ToHtml ()
import Text.Blaze.Html5
import Text.Blaze.Html.Renderer.Pretty (renderHtml)

import PrettyPrinting
import qualified CoreCollection as CC

import qualified CoreLang as CL

instance HasField (field :: k) r t => HasField field (Maybe r) (Maybe t) where
    getField = fmap (getField @field)
    

data PassView  = PassView { info :: CC.PassInfo
                          , prevPass :: Maybe PassView
                          , nextPass :: Maybe PassView
                          , filepath :: FilePath
                          , ast_filepath :: FilePath
                          }

data GlobalPassInfo = GlobalPassInfo { nrViews :: Int
                                     , cssPath :: FilePath
                                     }

collectInfo :: [PassView] -> GlobalPassInfo
collectInfo views = GlobalPassInfo { nrViews = length views
                                   , cssPath = "/tmp/hs-comprehension-style.css"
                                   }


infoToView :: CC.PassInfo -> IO PassView
infoToView info = do
    let idx = info.idx
    let path = [I.i|/tmp/hs-comprehension-#{idx}.html|]
    let ast_path = [I.i|/tmp/hs-comprehension-ast-#{idx}.html|]

    let code = info.ast
    withFile ast_path WriteMode $ \h -> hPutStr h [I.i|#{code}|]

    print code

    pure $ PassView { info = info
                    , prevPass = Nothing
                    , nextPass = Nothing
                    , filepath = path
                    , ast_filepath = ast_path
                    }


buttonToPass :: Maybe PassView -> Html
buttonToPass view = let
    filepath = fromMaybe "#" view.filepath
    title = fromMaybe "#" view.info.title

    in [hsx|<a class="pass-btn" href={takeFileName filepath}><button>{title}</button></a>|]

outputCss :: GlobalPassInfo -> IO ()
outputCss globals = let 
    css = [I.i|#{myCss}
               #{pygmentCss}
               #{ansiCss}]|]

    in withFile globals.cssPath WriteMode $ \h -> hPutStr h css

renderPass :: GlobalPassInfo -> PassView -> IO Html
renderPass globals view = do
    guts_colored <- highlight view.info.ast
    diff_colored <- case view.prevPass of
                        Nothing -> pure ""
                        Just prevPass -> diffFiles prevPass.ast_filepath view.ast_filepath
    let title = view.info.title
    let idx = view.info.idx
    let nrViews = globals.nrViews

    let prevPass = view.prevPass
    let nextPass = view.nextPass
    pure $ 
        [hsx|
        <html>
            <head>
                <title>{title}</title>
                <link rel="stylesheet" href="hs-comprehension-style.css">
            </head>
            <body>
                <h1>{title} {idx}/{nrViews}</h1>
                <div class="button-container">
                    {buttonToPass prevPass}
                    {buttonToPass nextPass}
                </div>
                <pre class="code">{preEscapedToHtml guts_colored}</pre>
                <pre class="diff">{preEscapedToHtml diff_colored}</pre>
            </body>
        </html>
        |]
        
saveToFile :: FilePath -> Html -> IO ()
saveToFile path html = 
    let 
        bs :: String
        bs = renderHtml html 
    in 
        withFile path WriteMode $ \handle -> do
            hPutStr handle bs
            putStrLn $ "file://" ++ path

highlight :: String -> IO String
highlight = readProcess "/home/hugo/repos/hs-comprehension/hs-comprehension/scripts/hightlight.py" []

diffFiles :: FilePath -> FilePath -> IO String
diffFiles lhs rhs = do
    ret <- readProcess "/home/hugo/repos/hs-comprehension/hs-comprehension/scripts/diff.sh" [lhs, rhs] mempty
    pure $ if ret == mempty then "\nThe files are the same" else "\n" ++ ret

myCss :: Html
myCss = preEscapedToHtml @String " .body { font-family: monospace, monospace; } .button-container { display: flex; } .pass-btn button { width: 100%; padding: 2em; font-size: 24px; } .pass-btn { width: 100%; } pre.code { color: #FFFFFF; background-color: #122a34; padding: 2em; font-size: 13pt; } pre.diff { color: #FFFFFF; background-color: #122a34; padding: 2em; font-size: 13pt; } "

pygmentCss :: Html
pygmentCss = preEscapedToHtml @String " .hll { backgroun17e } /* Comment */ .err { color: #960050; background-color: #1e0010 } /* Error */ .k { color: #66d9ef } /* Keyword */ .l { color: #ae81ff } /* Literal */ .n { color: #f8f8f2 } /* Name */ .o { color: #f92672 } /* Operator */ .p { color: #f8f8f2 } /* Punctuation */ .ch { color: #75715e } /* Comment.Hashbang */ .cm { color: #75715e } /* Comment.Multiline */ .cp { color: #75715e } /* Comment.Preproc */ .cpf { color: #75715e } /* Comment.PreprocFile */ .c1 { color: #95917e } /* Comment.Single */ .cs { color: #75715e } /* Comment.Special */ .gd { color: #f92672 } /* Generic.Deleted */ .ge { font-style: italic } /* Generic.Emph */ .gi { color: #a6e22e } /* Generic.Inserted */ .gs { font-weight: bold } /* Generic.Strong */ .gu { color: #75715e } /* Generic.Subheading */ .kc { color: #66d9ef } /* Keyword.Constant */ .kd { color: #66d9ef } /* Keyword.Declaration */ .kn { color: #f92672 } /* Keyword.Namespace */ .kp { color: #66d9ef } /* Keyword.Pseudo */ .kr { color: #66d9ef } /* Keyword.Reserved */ .kt { color: #66d9ef } /* Keyword.Type */ .ld { color: #e6db74 } /* Literal.Date */ .m { color: #ae81ff } /* Literal.Number */ .s { color: #e6db74 } /* Literal.String */ .na { color: #a6e22e } /* Name.Attribute */ .nb { color: #f8f8f2 } /* Name.Builtin */ .nc { color: #a6e22e } /* Name.Class */ .no { color: #66d9ef } /* Name.Constant */ .nd { color: #a6e22e } /* Name.Decorator */ .ni { color: #f8f8f2 } /* Name.Entity */ .ne { color: #a6e22e } /* Name.Exception */ .nf { color: #a6e22e } /* Name.Function */ .nl { color: #f8f8f2 } /* Name.Label */ .nn { color: #f8f8f2 } /* Name.Namespace */ .nx { color: #a6e22e } /* Name.Other */ .py { color: #f8f8f2 } /* Name.Property */ .nt { color: #f92672 } /* Name.Tag */ .nv { color: #f8f8f2 } /* Name.Variable */ .ow { color: #f92672 } /* Operator.Word */ .w { color: #f8f8f2 } /* Text.Whitespace */ .mb { color: #ae81ff } /* Literal.Number.Bin */ .mf { color: #ae81ff } /* Literal.Number.Float */ .mh { color: #ae81ff } /* Literal.Number.Hex */ .mi { color: #ae81ff } /* Literal.Number.Integer */ .mo { color: #ae81ff } /* Literal.Number.Oct */ .sa { color: #e6db74 } /* Literal.String.Affix */ .sb { color: #e6db74 } /* Literal.String.Backtick */ .dl { color: #e6db74 } /* Literal.String.Delimiter */ .sd { color: #e6db74 } /* Literal.String.Doc */ .s2 { color: #e6db74 } /* Literal.String.Double */ .se { color: #ae81ff } /* Literal.String.Escape */ .sh { color: #e6db74 } /* Literal.String.Heredoc */ .si { color: #e6db74 } /* Literal.String.Interpol */ .sx { color: #e6db74 } /* Literal.String.Other */ .sr { color: #e6db74 } /* Literal.String.Regex */ .s1 { color: #e6db74 } /* Literal.String.Single */ .ss { color: #e6db74 } /* Literal.String.Symbol */ .bp { color: #f8f8f2 } /* Name.Builtin.Pseudo */ .fm { color: #a6e22e } /* Name.Function.Magic */ .vc { color: #f8f8f2 } /* Name.Variable.Class */ .vg { color: #f8f8f2 } /* Name.Variable.Global */ .vi { color: #f8f8f2 } /* Name.Variable.Instance */ .vm { color: #f8f8f2 } /* Name.Variable.Magic */ .il { color: #ae81ff } /* Literal.Number.Integer.Long */"

ansiCss :: Html
ansiCss = ""
