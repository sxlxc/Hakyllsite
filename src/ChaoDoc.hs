{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module ChaoDoc (chaoDocRead, chaoDocWrite, chaoDocPandocCompiler, chaoDocCompiler) where

import Control.Monad.State
import Data.Either
import Data.Functor
import Data.List (intersect)
import qualified Data.Map as M
import Data.Maybe
import Data.Text (Text, pack)
import qualified Data.Text as T
import Hakyll
import Pangu (isCJK, pangu)
import SideNoteHTML (usingSideNotesHTML)
import System.IO.Unsafe
import Text.Pandoc
-- import Text.Pandoc.Builder
import Text.Pandoc.Citeproc
import Text.Pandoc.Walk (query, walk, walkM)

-- setMeta key val (Pandoc (Meta ms) bs) = Pandoc (Meta $ M.insert key val ms) bs

-- On mac, please do `export LANG=C` before using this thing
chaoDocRead :: ReaderOptions
chaoDocRead =
  def
    { readerExtensions =
        enableExtension Ext_tex_math_double_backslash $
        enableExtension Ext_tex_math_single_backslash $
        enableExtension Ext_latex_macros              $
        enableExtension Ext_raw_tex pandocExtensions
    }

chaoDocWrite :: WriterOptions
chaoDocWrite =
  def
    { writerHTMLMathMethod = MathML,
      -- writerHtml5          = True,
      -- writerHighlightStyle = Just syntaxHighlightingStyle,
      writerNumberSections = True,
      writerTableOfContents = True,
      writerTOCDepth = 2
    }

-- getInline :: Inline -> [Inline]
-- getInline x = [x]

pandocToInline :: Pandoc -> [Inline]
pandocToInline (Pandoc _ blocks) = go (reverse blocks)
  where
    go (Plain inlines : _) = inlines
    go (Para inlines : _) = inlines
    go (_ : xs) = go xs
    go [] = []

incrementalBlock :: [Text]
incrementalBlock =
  [ "Theorem",
    "Conjecture",
    "Definition",
    "Example",
    "Lemma",
    "Problem",
    "Proposition",
    "Corollary",
    "Observation",
    "定理",
    "猜想",
    "定义",
    "例",
    "引理",
    "问题",
    "命题",
    "推论",
    "观察"
  ]

otherBlock :: [Text]
otherBlock = ["Proof", "Remark", "证明", "备注"]

theoremClasses :: [Text]
theoremClasses = incrementalBlock ++ otherBlock

-- create a filter for theorems
getClass :: Attr -> [Text]
getClass (_, c, _) = c

addClass :: Attr -> Text -> Attr
addClass (a, b, c) d = (a, d : b, c)

addAttr :: Attr -> Text -> Text -> Attr
addAttr (a, b, c) x y = (a, b, (x, y) : c)

-- For each theorem, add a number, and also add add class theorem
preprocessTheorems :: Block -> State Int Block
preprocessTheorems (Div attr xs)
  | isIncremental = do
      curId <- get
      put (curId + 1)
      return $ Div (addAttr attr' "index" (pack $ show curId)) xs
  | isOtherBlock = return $ Div attr' xs
  | otherwise = return (Div attr xs)
  where
    isIncremental = getClass attr `intersect` incrementalBlock /= []
    isOtherBlock = getClass attr `intersect` otherBlock /= []
    theoremType = head (getClass attr `intersect` theoremClasses)
    attr' = addAttr attr "type" theoremType
preprocessTheorems x = return x

theoremFilter :: Pandoc -> Pandoc
theoremFilter doc = walk makeTheorem $ autorefFilter $ evalState (walkM preprocessTheorems doc) 1

--    [index, type, idx]
theoremIndex :: Block -> [(Text, (Text, Text))]
theoremIndex (Div attr _)
  | isNothing t = []
  | isIncremental = [(idx, (fromJust t, fromJust index))]
  | otherwise = []
  where
    (idx, _, parm) = attr
    t = lookup "type" parm
    index = lookup "index" parm
    isIncremental = fromJust t `elem` incrementalBlock
theoremIndex _ = []

autoref :: [(Text, (Text, Text))] -> Inline -> Inline
autoref x (Cite citations inlines)
  | valid = Link nullAttr [Str linkTitle] ("#" <> citeid, linkTitle)
  | otherwise = Cite citations inlines
  where
    citeid = citationId $ head citations
    valid = citeid `elem` map fst x
    (theoremType, num) = fromJust $ lookup citeid x
    linkTitle = theoremType <> " " <> num
autoref _ y = y

autorefFilter :: Pandoc -> Pandoc
autorefFilter x = walk (autoref links) x
  where
    links = query theoremIndex x

-- processCitations works on AST. If you want to use citations in theorem name,
-- then you need to convert citations there to AST as well and then use processCitations\
-- Thus one need to apply the theorem filter first.
-- autoref still does not work.
mathMacros :: Text
mathMacros = unsafePerformIO (pack <$> readFile "math-macros.tex")
{-# NOINLINE mathMacros #-}

prependMacros :: Text -> Text -> Text
prependMacros macros body = macros <> "\n\n" <> body

prependMathMacros :: Text -> Text
prependMathMacros = prependMacros mathMacros

thmNamePandoc :: Text -> Pandoc
thmNamePandoc x =
  fromRight (Pandoc nullMeta []) . runPure $
    readMarkdown chaoDocRead (prependMathMacros x)

makeTheorem :: Block -> Block
makeTheorem (Div attr xs)
  | isNothing t = Div attr xs
  | otherwise = Div (addClass attr "theorem-environment") (Plain [header] : xs)
  where
    (_, _, parm) = attr
    t = lookup "type" parm
    name = lookup "title" parm
    index = lookup "index" parm
    header = Span (addClass nullAttr "theorem-header") [typetext, indextext, nametext]
    typetext = Span (addClass nullAttr "type") [Str $ fromJust t]
    indextext =
      if isNothing index
        then Str ""
        else Span (addClass nullAttr "index") [Str $ fromJust index]
    nametext =
      if isNothing name
        then Str ""
        else Span (addClass nullAttr "name") (pandocToInline $ thmNamePandoc $ fromJust name)
makeTheorem x = x

-- bib from https://github.com/chaoxu/chaoxu.github.io/tree/develop
cslFile :: String
cslFile = "bib_style.csl"

bibFile :: String
bibFile = "reference.bib"

chaoDocPandocCompiler :: Compiler (Item Pandoc)
chaoDocPandocCompiler = do
  macros <- T.pack <$> loadBody "math-macros.tex"
  body <- getResourceBody
  let bodyWithMacros =
        fmap (T.unpack . prependMacros macros . T.pack) body
  myReadPandocBiblio chaoDocRead (T.pack cslFile) (T.pack bibFile) myFilter bodyWithMacros

chaoDocCompiler :: Compiler (Item String)
chaoDocCompiler = chaoDocPandocCompiler <&> writePandocWith chaoDocWrite

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
myFilter = usingSideNotesHTML chaoDocWrite . theoremFilter . panguFilter . displayMathFilter

-- pangu filter
lastChar :: Inline -> Maybe Char
lastChar e = case e of
  Str s -> if null (T.unpack s) then Nothing else Just (last (T.unpack s))
  Emph is -> lastCharList is
  Strong is -> lastCharList is
  Strikeout is -> lastCharList is
  Link _ is _ -> lastCharList is
  Span _ is -> lastCharList is
  Quoted _ is -> lastCharList is
  _ -> Nothing
  where
    lastCharList [] = Nothing
    lastCharList is = lastChar (last is)

firstChar :: Inline -> Maybe Char
firstChar e = case e of
  Str s -> if null (T.unpack s) then Nothing else Just (head (T.unpack s))
  Emph is -> firstCharList is
  Strong is -> firstCharList is
  Strikeout is -> firstCharList is
  Link _ is _ -> firstCharList is
  Span _ is -> firstCharList is
  Quoted _ is -> firstCharList is
  _ -> Nothing
  where
    firstCharList [] = Nothing
    firstCharList is = firstChar (head is)

panguInline :: Inline -> Inline
panguInline e = case e of
  Str s -> Str (pangu s)
  Emph is -> Emph (panguInlines is)
  Strong is -> Strong (panguInlines is)
  Strikeout is -> Strikeout (panguInlines is)
  Link at is tg -> Link at (panguInlines is) tg
  Span at is -> Span at (panguInlines is)
  Quoted qt is -> Quoted qt (panguInlines is)
  _ -> e

panguInlines :: [Inline] -> [Inline]
panguInlines = foldr (addSpace . panguInline) []
  where
    addSpace x [] = [x]
    addSpace x (y : ys)
      | shouldSpace x y = x : Space : y : ys
      | otherwise = x : y : ys
    shouldSpace x y = case (lastChar x, firstChar y) of
      (Just lc, Just fc) -> isCJK lc /= isCJK fc
      _ -> False

panguFilter :: Pandoc -> Pandoc
panguFilter = walk transformBlocks
  where
    transformBlocks :: Block -> Block
    transformBlocks (Para inlines) = Para (panguInlines inlines)
    transformBlocks x = x

-- display math wrapper for MathML
displayMathFilter :: Pandoc -> Pandoc
displayMathFilter = walk wrapDisplayMath
  where
    wrapDisplayMath m@(Math DisplayMath _) =
      Span ("math-container", [], []) [m]
    wrapDisplayMath x = x
