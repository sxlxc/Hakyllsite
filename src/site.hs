--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

-- import Data.Monoid (mappend)
-- import Hakyll
-- import Text.Pandoc
-- import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Text as T
-- import System.Process (readProcess)
import Text.Pandoc.Definition (Block (CodeBlock, RawBlock), Pandoc)
import Text.Pandoc.Walk (walk, walkM)
import ChaoDoc
import           Data.Monoid (mappend, Monoid (..))
import           Hakyll
import           Hakyll.Core.Compiler
import           Hakyll.Web.Html
import           System.FilePath.Posix
import           Text.Pandoc
import           Control.Monad
import           Control.Applicative        ((<$>), Alternative (..), (<$>))
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text, unpack, pack)
import qualified Data.Map as M
import           Hakyll.Web.Pandoc.Biblio 
-- import qualified Text.CSL as CSL
import           Text.Pandoc.Options
import           Text.Pandoc.Citeproc
import           System.IO.Unsafe
import           Data.Either
--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  match "images/**" $ do
    route idRoute
    compile copyFileCompiler

  match "pdfs/**" $ do
    route idRoute
    compile copyFileCompiler

  match "fonts/*" $ do
    route idRoute
    compile copyFileCompiler

  match "favicon.ico" $ do
    route idRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match "about.md" $ do
    route $ setExtension "html"
    compile $
      chaoDocCompiler
        >>= loadAndApplyTemplate "templates/about.html" defaultContext
        >>= relativizeUrls

  -- build up tags
  tags <- buildTags "posts/*" (fromCapture "tags/*.html")
  tagsRules tags $ \tag pattern -> do
    let title = "Posts tagged \"" ++ tag ++ "\""
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll pattern
      let ctx =
            constField "title" title
              `mappend` listField "posts" (postCtxWithTags tags) (return posts)
              `mappend` defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/tag.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx
        >>= relativizeUrls

  match "posts/*" $ do
    route $ setExtension "html"
    compile $
      chaoDocCompiler
        >>= loadAndApplyTemplate "templates/post.html" (postCtxWithTags tags)
        >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
        >>= relativizeUrls
        >>= katexFilter

  create ["archive.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let archiveCtx =
            listField "posts" postCtx (return posts)
              `mappend` constField "title" "Archives"
              `mappend` defaultContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- fmap (take 20) . recentFirst =<< loadAll "posts/*"
      let indexCtx =
            listField "posts" postCtx (return posts)
              `mappend` defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/index.html" indexCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    `mappend` defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx

pandocCompiler_ :: Compiler (Item String)
pandocCompiler_ =
    let
    mathExtensions =
        [ Ext_tex_math_dollars
        , Ext_tex_math_double_backslash
        , Ext_latex_macros
        , Ext_raw_tex
        , Ext_raw_html
        ]
    newExtensions = foldr enableExtension defaultExtensions mathExtensions
    defaultExtensions = writerExtensions defaultHakyllWriterOptions
    writerOptions =
        defaultHakyllWriterOptions
        { writerExtensions = newExtensions
        , writerHTMLMathMethod = KaTeX ""
        }
    in pandocCompilerWithTransformM defaultHakyllReaderOptions writerOptions pygmentsHighlight
    where 
      pygmentsHighlight :: Pandoc -> Compiler Pandoc
      pygmentsHighlight = walkM \case
        CodeBlock (_, listToMaybe -> mbLang, _) (T.unpack -> body) -> do
          let lang = T.unpack (fromMaybe "text" mbLang)
          RawBlock "html" . T.pack <$> callPygs lang body
        block -> pure block
        where
          callPygs :: String -> String -> Compiler String
          callPygs lang = unixFilter "pygmentize" [ "-l", lang
                                                  , "-f", "html"
                                                  , "-P", "cssclass=pygmentize-block"
                                                  , "-P", "cssstyles=padding-left: 1em;"
                                                  ]


katexFilter :: Item String -> Compiler (Item String)
katexFilter = withItemBody (unixFilter "./katex_cli" [])

-- copied from chao's site.hs for biblography
cslFile = "bib_style.csl" 
bibFile = "reference.bib" 

chaoDocCompiler :: Compiler (Item String)
chaoDocCompiler = do
    getResourceBody >>=
        myReadPandocBiblio chaoDocRead (pack cslFile) (pack bibFile) theoremFilter >>=
        return . writePandocWith chaoDocWrite
addMeta name value (Pandoc meta a) =
  let prevMap = unMeta meta
      newMap = M.insert name value prevMap
      newMeta = Meta newMap
  in  Pandoc newMeta a

myReadPandocBiblio :: ReaderOptions
                   -> T.Text  -- csl file name
                   -> T.Text
                   -> (Pandoc -> Pandoc)           -- apply a filter before citeproc
                   -> Item String
                   -> Compiler (Item Pandoc)
myReadPandocBiblio ropt csl biblio filter item  = do
    -- Parse CSL file, if given
    -- style <- unsafeCompiler $ CSL.readCSLFile Nothing . toFilePath . itemIdentifier $ csl

    -- We need to know the citation keys, add then *before* actually parsing the
    -- actual page. If we don't do this, pandoc won't even consider them
    -- citations!
    -- let Biblio refs = itemBody biblio
    pandoc <- itemBody <$> readPandocWith ropt item
    let pandoc' = fromRight pandoc $ unsafePerformIO $ runIO $ processCitations $ addMeta "bibliography" (MetaList [MetaString biblio]) $ 
                  addMeta "csl" (MetaString csl) $ 
                  addMeta "link-citations" (MetaBool True) $ 
                  addMeta "reference-section-title" (MetaInlines [Str "References"]) $
                  filter pandoc -- here's the change
    --let a x = itemSetBody (pandoc' x) 
    return $ fmap (const pandoc') item
