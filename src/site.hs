{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

import ChaoDoc
import qualified Data.Text as T
import Hakyll
import System.FilePath
import Text.Pandoc

--------------------------------------------------------------------------------
-- https://www.rohanjain.in/hakyll-clean-urls/
cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory p </> takeBaseName p </> "index.html"
      where
        p = toFilePath ident

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll pattern replacement)
  where
    pattern :: String = "/index.html"
    replacement :: String -> String = const "/"

--------------------------------------------------------------------------------

main :: IO ()
main = hakyll $ do
  match "images/**" $ do
    route idRoute
    compile copyFileCompiler

  -- match "katex/**" $ do
  --   route idRoute
  --   compile copyFileCompiler

  match "mathjax/**" $ do
    route idRoute
    compile copyFileCompiler

  match "mathjax-fira-font/**" $ do
    route idRoute
    compile copyFileCompiler

  match "fonts/*" $ do
    route idRoute
    compile copyFileCompiler

  match "favicon.ico" $ do
    route idRoute
    compile copyFileCompiler

  match "404.html" $ do
    route cleanRoute
    compile copyFileCompiler

  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match "about.md" $ do
    route cleanRoute
    compile $
      chaoDocCompiler
        >>= loadAndApplyTemplate "templates/about.html" defaultContext
        >>= relativizeUrls

  -- build up tags
  tags <- buildTags "posts/*" (fromCapture "tags/*.html")
  tagsRules tags $ \tag pattern -> do
    let title = "Posts tagged \"" ++ tag ++ "\""
    route cleanRoute
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
    route cleanRoute
    compile $ do
      makeItem ""
        >>= loadAndApplyTemplate "templates/tags.html" (defaultCtxWithTags tags)
        >>= loadAndApplyTemplate "templates/default.html" (defaultCtxWithTags tags)

  match "posts/*" $ do
    route cleanRoute
    compile $ do
      tocCtx <- getTocCtx (postCtxWithTags tags)
      chaoDocCompiler
        >>= loadAndApplyTemplate "templates/post.html" tocCtx
        >>= loadAndApplyTemplate "templates/default.html" tocCtx
        >>= relativizeUrls
  --    >>= katexFilter

  match "standalone/*" $ do
    route cleanRoute
    compile $ do
      tocCtx <- getTocCtx (postCtxWithTags tags)
      chaoDocCompiler
        >>= loadAndApplyTemplate "templates/standalone.html" tocCtx
        >>= relativizeUrls

  create ["notes.html"] $ do
    route cleanRoute
    compile $ do
      notes <- recentFirst =<< loadAll "standalone/*"
      let notesCtx =
            listField "posts" postCtx (return notes)
              `mappend` constField "title" "Notes"
              `mappend` defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/notes.html" notesCtx
        >>= loadAndApplyTemplate "templates/index.html" notesCtx
        >>= relativizeUrls
        >>= cleanIndexHtmls

  create ["archive.html"] $ do
    route cleanRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let archiveCtx =
            listField "posts" postCtx (return posts)
              `mappend` constField "title" "Archives"
              `mappend` defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
        >>= loadAndApplyTemplate "templates/index.html" archiveCtx
        >>= relativizeUrls
        >>= cleanIndexHtmls

  create ["draft.html"] $ do
    route cleanRoute
    compile $ do
      posts <- recentFirst =<< loadAll "posts/*"
      let draftCtx =
            listField "posts" postCtx (return posts)
              `mappend` constField "title" "Drafts"
              `mappend` defaultContext
      makeItem ""
        >>= loadAndApplyTemplate "templates/draft.html" draftCtx
        >>= loadAndApplyTemplate "templates/index.html" draftCtx
        >>= relativizeUrls
        >>= cleanIndexHtmls

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
        >>= cleanIndexHtmls

  match "templates/*" $ compile templateBodyCompiler

-- https://robertwpearce.com/hakyll-pt-2-generating-a-sitemap-xml-file.html
-- create ["sitemap.xml"] $ do
--   route idRoute
--   compile $ do
--     posts <- recentFirst =<< loadAll "posts/*"
--     singlePages <- loadAll (fromList ["about.md"])
--     let pages = posts <> singlePages
--         sitemapCtx =
--           constField "root" root
--             <> listField "pages" postCtx (return pages) -- here
--     makeItem ""
--       >>= loadAndApplyTemplate "templates/sitemap.xml" sitemapCtx

--------------------------------------------------------------------------------

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y"
    -- <> constField "root" root
    <> dateField "date" "%Y-%m-%d"
    <> defaultContext

postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags `mappend` postCtx

defaultCtxWithTags :: Tags -> Context String
defaultCtxWithTags tags = listField "tags" tagsCtx getAllTags <> defaultContext
  where
    getAllTags :: Compiler [Item (String, [Identifier])]
    getAllTags = pure . map mkItem $ tagsMap tags
      where
        mkItem :: (String, [Identifier]) -> Item (String, [Identifier])
        mkItem x@(t, _) = Item (tagsMakeId tags t) x
    tagsCtx =
      listFieldWith "posts" (postCtxWithTags tags) getPosts
        <> metadataField
        <> urlField "url"
        <> pathField "path"
        <> titleField "title"
        <> missingField
      where
        getPosts ::
          Item (String, [Identifier]) ->
          Compiler [Item String]
        getPosts (itemBody -> (_, is)) = mapM load is

-- toc from https://github.com/slotThe/slotThe.github.io
getTocCtx :: Context a -> Compiler (Context a)
getTocCtx ctx = do
  noToc <- (Just "true" ==) <$> (getUnderlying >>= (`getMetadataField` "no-toc"))
  writerOpts <- mkTocWriter defaultHakyllWriterOptions
  toc <- renderPandocWith chaoDocRead writerOpts =<< getResourceBody
  pure $
    mconcat
      [ ctx,
        constField "toc" $ killLinkIds (itemBody toc),
        if noToc then boolField "no-toc" (pure noToc) else mempty
      ]
  where
    mkTocWriter :: WriterOptions -> Compiler WriterOptions
    mkTocWriter writerOpts = do
      tmpl <- either (const Nothing) Just <$> unsafeCompiler (compileTemplate "" "$toc$")
      pure $
        writerOpts
          { writerTableOfContents = True,
            writerTOCDepth = 2,
            writerTemplate = tmpl,
            writerHTMLMathMethod = MathJax ""
          }

    asTxt :: (T.Text -> T.Text) -> String -> String
    asTxt f = T.unpack . f . T.pack

    killLinkIds :: String -> String
    killLinkIds = asTxt (mconcat . go . T.splitOn "id=\"toc-")
      where
        go :: [T.Text] -> [T.Text]
        go = \case
          [] -> []
          x : xs -> x : map (T.drop 1 . T.dropWhile (/= '\"')) xs

-- katexFilter :: Item String -> Compiler (Item String)
-- katexFilter = withItemBody (unixFilter "./katex_cli" [])
