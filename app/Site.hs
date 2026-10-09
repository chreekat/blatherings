{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.List (isPrefixOf)
import Hakyll
import System.Environment (lookupEnv)
import System.FilePath (takeDirectory, (</>))
import Text.Pandoc.Extensions (Extension (Ext_blank_before_blockquote), disableExtension)
import Text.Pandoc.Options (HTMLMathMethod (MathML), ReaderOptions (..), WriterOptions (..))

import Blatherings

-- | Whether drafts become pages.
data Visibility = PublishedOnly | WithDrafts

main :: IO ()
main = do
    drafts <- lookupEnv "DRAFTS"
    let visibility = if drafts == Just "1" then WithDrafts else PublishedOnly
    hakyllWith config (rules visibility)

config :: Configuration
config = defaultConfiguration
    { ignoreFile = \f -> ignoreFile defaultConfiguration f
        || f `elem` ["dist-newstyle", "result"]
    }

root :: String
root = "https://words.chreekat.net"

feedConfig :: String -> FeedConfiguration
feedConfig title = FeedConfiguration
    { feedTitle = title
    , feedDescription = "Writing by chreekat"
    , feedAuthorName = "Bryan Richter"
    , feedAuthorEmail = "b@chreekat.net"
    , feedRoot = root
    }

rules :: Visibility -> Rules ()
rules visibility = do
    let posts = case visibility of
            PublishedOnly -> "posts/*"
            WithDrafts -> "posts/*" .||. "drafts/*"

    match "templates/*" $ compile templateBodyCompiler

    match "css/*" $ do
        route idRoute
        compile compressCssCompiler

    match "static/**" $ do
        route $ gsubRoute "static/" (const "")
        compile copyFileCompiler

    match "images/**" $ do
        route idRoute
        compile copyFileCompiler

    tags <- buildTags posts (\t -> fromFilePath ("tags" </> tagSlug t </> "index.html"))
    let ctx = postCtx tags

    match posts $ do
        route $ customRoute (either error id . postPath . toFilePath)
        compile $ pandocCompilerWith readerOptions writerOptions
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/post.html" ctx
            >>= page ctx

    tagsRules tags $ \tag tagged -> do
        route idRoute
        compile $ do
            items <- recentFirst =<< loadAll tagged
            let tagCtx = constField "title" ("Tagged “" <> escapeHtml tag <> "”")
                    <> constField "feed" (toUrl (toFilePath (tagFeedId tags tag)))
                    <> listField "posts" ctx (pure items)
                    <> siteCtx
            makeItem ""
                >>= loadAndApplyTemplate "templates/post-list.html" tagCtx
                >>= page tagCtx

    let tagFeeds = [(tagFeedId tags tag, (tag, ids)) | (tag, ids) <- tagsMap tags]
    create (map fst tagFeeds) $ do
        route idRoute
        compile $ do
            ident <- getUnderlying
            (tag, ids) <- maybe (noResult "not a tag feed") pure (lookup ident tagFeeds)
            items <- loadFeedItems (fromList ids)
            renderAtom (feedConfig ("words: " <> tag)) (feedCtx ctx) items

    create ["tags/index.html"] $ do
        route idRoute
        compile $ do
            cloud <- renderTagList (sortTagsBy caseInsensitiveTags tags)
            let tagsCtx = constField "title" "Tags" <> constField "tags" cloud <> siteCtx
            makeItem ""
                >>= loadAndApplyTemplate "templates/tags.html" tagsCtx
                >>= page tagsCtx

    create ["index.html"] $ do
        route idRoute
        compile $ do
            items <- recentFirst =<< loadAll posts
            let indexCtx = boolField "home" (const True)
                    <> constField "title" "Writing"
                    <> listField "posts" ctx (pure items)
                    <> siteCtx
            makeItem ""
                >>= loadAndApplyTemplate "templates/post-list.html" indexCtx
                >>= page indexCtx

    create ["atom.xml"] $ do
        route idRoute
        compile $ loadFeedItems posts >>= renderAtom (feedConfig "words") (feedCtx ctx)

    create ["404.html"] $ do
        route idRoute
        compile $ do
            let notFoundCtx = constField "title" "Not found" <> siteCtx
            makeItem ""
                >>= loadAndApplyTemplate "templates/404.html" notFoundCtx
                >>= page notFoundCtx
  where
    loadFeedItems pat = fmap (take 20) . recentFirst =<< loadAllSnapshots pat "content"

-- | A tag's Atom feed, beside its page.
tagFeedId :: Tags -> String -> Identifier
tagFeedId tags tag = fromFilePath (takeDirectory (toFilePath (tagsMakeId tags tag)) </> "atom.xml")

-- | Front-matter text, safe in HTML and XML.
escapedMetadataField :: String -> Context a
escapedMetadataField key = field key $ \i ->
    maybe (noResult ("no " <> key)) (pure . escapeHtml) =<< getMetadataField (itemIdentifier i) key

-- | The site chrome around any page, with tidy links.
page :: Context String -> Item String -> Compiler (Item String)
page ctx item = do
    full <- loadAndApplyTemplate "templates/default.html" ctx item
    pure (withUrls prettyUrl <$> full)

-- | Pandoc Markdown, accepting GitHub's habit of quoting without a blank line first.
readerOptions :: ReaderOptions
readerOptions = defaultHakyllReaderOptions
    { readerExtensions = disableExtension Ext_blank_before_blockquote
        (readerExtensions defaultHakyllReaderOptions)
    }

writerOptions :: WriterOptions
writerOptions = defaultHakyllWriterOptions { writerHTMLMathMethod = MathML }

siteCtx :: Context String
siteCtx = constField "root" root
    <> foldMap escapedMetadataField ["title", "description"]
    <> field "cssVersion" (const (fingerprint . itemBody <$> load "css/main.css"))
    <> prettyUrlField
    <> defaultContext

postCtx :: Tags -> Context String
postCtx tags = dateField "date" "%-d %B %Y"
    <> dateField "isodate" "%Y-%m-%d"
    <> tagsField "tags" tags
    <> boolField "draft" (("drafts/" `isPrefixOf`) . toFilePath . itemIdentifier)
    <> siteCtx

-- | Atom entries carry full content with links that work off-site.
feedCtx :: Context String -> Context String
feedCtx ctx = field "description" (pure . withUrls (absoluteUrl root) . itemBody)
    <> ctx

-- | @url@, without @index.html@.
prettyUrlField :: Context a
prettyUrlField = field "url" $ \i ->
    maybe (noResult "no route") (pure . prettyUrl . toUrl) =<< getRoute (itemIdentifier i)
