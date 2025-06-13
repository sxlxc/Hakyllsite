{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

import ChaoDoc
import Data.Either
import Data.Functor
import qualified Data.Map as M
import qualified Data.Text as T
import Hakyll
import System.IO.Unsafe
import Text.Pandoc
import Text.Pandoc.Citeproc

root :: String
root = "https://talldoor.uk"
--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
  match "images/**" $ do
    route idRoute
    compile copyFileCompiler

  match "katex/**" $ do
    route idRoute
    compile copyFileCompiler

  -- match "fonts/*" $ do
  --   route idRoute
  --   compile copyFileCompiler

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
  match "404.md" $ do
    route $ setExtension "html"
    compile $
      chaoDocCompiler
        >>= loadAndApplyTemplate "templates/default.html" defaultContext
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

  create ["tags.html"] $ do
    route idRoute
    compile $ do
      makeItem ""
        >>= loadAndApplyTemplate "templates/tags.html" (defaultCtxWithTags tags)
        >>= loadAndApplyTemplate "templates/default.html" (defaultCtxWithTags tags)


  match "posts/*" $ do
    route $ setExtension "html"
    compile $ do
      tocCtx <- getTocCtx (postCtxWithTags tags)
      chaoDocCompiler
        >>= loadAndApplyTemplate "templates/post.html" tocCtx
        >>= loadAndApplyTemplate "templates/default.html" tocCtx
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

  create ["draft.html"] $ do
    route idRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let archiveCtx =
            listField "posts" postCtx (return posts)
              `mappend` constField "title" "Drafts"
              `mappend` defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/draft.html" archiveCtx
        >>= loadAndApplyTemplate "templates/default.html" archiveCtx
        >>= relativizeUrls

  match "index.html" $ do
    route idRoute
    compile $ do
      posts <- fmap (take 25) . recentFirst =<< loadAll "posts/*"
      let indexCtx =
            listField "posts" postCtx (return posts)
              `mappend` defaultContext

      getResourceBody
        >>= applyAsTemplate indexCtx
        >>= loadAndApplyTemplate "templates/index.html" indexCtx
        >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler
  -- https://robertwpearce.com/hakyll-pt-2-generating-a-sitemap-xml-file.html
  create ["sitemap.xml"] $ do
      route idRoute
      compile $ do
          posts <- recentFirst =<< loadAll "posts/*"
          singlePages <- loadAll (fromList ["about.md"])
          let pages = posts <> singlePages
              sitemapCtx =
                  constField "root" root <> -- here
                  listField "pages" postCtx (return pages)
          makeItem ""
              >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
  <> constField "root" root
  <> dateField "date" "%Y-%m-%d"
  <> defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx

defaultCtxWithTags :: Tags -> Context String
defaultCtxWithTags tags = listField "tags" tagsCtx getAllTags <> defaultContext
   where  getAllTags :: Compiler [Item (String, [Identifier])]
          getAllTags = pure . map mkItem $ tagsMap tags
            where mkItem :: (String, [Identifier]) -> Item (String, [Identifier])
                  mkItem x@(t, _) = Item (tagsMakeId tags t) x
          tagsCtx = listFieldWith "posts" (postCtxWithTags tags) getPosts   <>
            metadataField                                     <>
            urlField "url"                                    <>
            pathField "path"                                  <>
            titleField "title"                                <>
            missingField
              where getPosts :: Item (String, [Identifier])
                         -> Compiler [Item String]
                    getPosts (itemBody -> (_, is)) = mapM load is

getTocCtx :: Context a -> Compiler (Context a)
getTocCtx ctx = do
  noToc      <- (Just "true" ==) <$> (getUnderlying >>= (`getMetadataField` "no-toc"))
  writerOpts <- mkTocWriter defaultHakyllWriterOptions
  toc        <- renderPandocWith chaoDocRead writerOpts =<< getResourceBody
  pure $ mconcat [ ctx
                 , constField "toc" $ killLinkIds (itemBody toc)
                 , if noToc then boolField "no-toc" (pure noToc) else mempty
                 ]
 where
  mkTocWriter :: WriterOptions -> Compiler WriterOptions
  mkTocWriter writerOpts = do
    tmpl <- either (const Nothing) Just <$> unsafeCompiler (compileTemplate "" "$toc$")
    pure $ writerOpts
      { writerTableOfContents = True
      , writerTOCDepth        = 2
      , writerTemplate        = tmpl
      , writerHTMLMathMethod  = KaTeX ""
      }
  
  asTxt :: (T.Text -> T.Text) -> String -> String
  asTxt f = T.unpack . f . T.pack
  
  killLinkIds :: String -> String
  killLinkIds = asTxt (mconcat . go . T.splitOn "id=\"toc-")
   where
    go :: [T.Text] -> [T.Text]
    go = \case
      []     -> []
      x : xs -> x : map (T.drop 1 . T.dropWhile (/= '\"')) xs

katexFilter :: Item String -> Compiler (Item String)
katexFilter = withItemBody (unixFilter "./katex_cli" [])

-- copied from chao's site.hs for biblography
cslFile :: String
cslFile = "bib_style.csl"

bibFile :: String
bibFile = "reference.bib"

chaoDocCompiler :: Compiler (Item String)
chaoDocCompiler = do
  ( getResourceBody
      >>= myReadPandocBiblio chaoDocRead (T.pack cslFile) (T.pack bibFile) myFilter
    )
    <&> writePandocWith chaoDocWrite

addMeta :: T.Text -> MetaValue -> Pandoc -> Pandoc
addMeta name value (Pandoc meta a) =
  let prevMap = unMeta meta
      newMap = M.insert name value prevMap
      newMeta = Meta newMap
   in Pandoc newMeta a

myReadPandocBiblio ::
  ReaderOptions ->
  T.Text -> -- csl file name
  T.Text ->
  (Pandoc -> Pandoc) -> -- apply a filter before citeproc
  Item String ->
  Compiler (Item Pandoc)
myReadPandocBiblio ropt csl biblio pdfilter item = do
  -- Parse CSL file, if given
  -- style <- unsafeCompiler $ CSL.readCSLFile Nothing . toFilePath . itemIdentifier $ csl

  -- We need to know the citation keys, add then *before* actually parsing the
  -- actual page. If we don't do this, pandoc won't even consider them
  -- citations!
  -- let Biblio refs = itemBody biblio
  pandoc <- itemBody <$> readPandocWith ropt item
  let pandoc' =
        fromRight pandoc $
          unsafePerformIO $
            runIO $
              processCitations $
                addMeta "bibliography" (MetaList [MetaString biblio]) $
                  addMeta "csl" (MetaString csl) $
                    addMeta "link-citations" (MetaBool True) $
                      addMeta "reference-section-title" (MetaInlines [Str "References"]) $
                        pdfilter pandoc -- here's the change
                        -- let a x = itemSetBody (pandoc' x)
  return $ fmap (const pandoc') item

myFilter :: Pandoc -> Pandoc
myFilter = theoremFilter