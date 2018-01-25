--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Monad (liftM)
import           Data.Monoid (mappend, mconcat)
import           Hakyll
import           Hakyll.Core.Store
import           Hakyll.Core.Compiler.Internal

import Text.CSL
import Text.CSL.Style hiding (match)
import Text.CSL.Reference
import qualified Text.CSL.Output.Pandoc as CSL.Pandoc
import Text.Pandoc


myTransportConfiguration :: Configuration
myTransportConfiguration = defaultConfiguration
    { deployCommand   = "rsync --checksum -ave 'ssh' _site/* aidan@phoric.eu:~/www"
    }

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Ontology Engineering with Diagrams"
    , feedDescription = "Technical posts regarding ontology engineering with diagrams"
    , feedAuthorName  = "Aidan Delaney"
    , feedAuthorEmail = "aidan@ontologyengineering.org"
    , feedRoot        = "http://www.phoric.eu/"
    }


--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith myTransportConfiguration $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match "bib/*" $ compile biblioCompiler
    match "csl/*" $ compile cslCompiler

    match (fromList ["about.markdown", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    create ["index.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Home"                `mappend`
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    create ["publications.html"] $ do
         route idRoute
         compile $ do
             csl <- load $ fromFilePath "csl/acm-sig-proceedings-long-author-list.csl"
             bib <- load $ fromFilePath "bib/all.bib"
             html <- loadAndApplyTemplate "templates/publications.html" (referencesContext csl) bib
             relativizeUrls (Item "publications.html" (itemBody html))

    -- Render RSS feed (from Eric)
    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend` bodyField "description"
            posts   <- recentFirst =<<  loadAllSnapshots "posts/*" "content"
            renderRss myFeedConfiguration feedCtx posts

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

isArticleJournal :: Reference -> Bool
isArticleJournal ref = refType ref == ArticleJournal

isPaperConference :: Reference -> Bool
isPaperConference ref = refType ref == PaperConference

isBook :: Reference -> Bool
isBook ref = refType ref == Book

styleCompiler :: Item CSL -> Compiler (Item Style)
styleCompiler csl = do
    style <- unsafeCompiler
             $ readCSLFile Nothing . toFilePath . itemIdentifier $ csl
    makeItem style

readBiblio :: Item Biblio -> Compiler (Item [Reference])
readBiblio ibiblio = makeItem refs where Biblio refs = itemBody ibiblio

readPandocReferences :: Item CSL -> Item [Reference] -> Compiler (Item Pandoc)
readPandocReferences icsl irefs = do
    istyle <- styleCompiler icsl
    let style = itemBody istyle
        refs = itemBody irefs
        formatted = processBibliography procOpts style refs
        blocks = map (return . Plain . CSL.Pandoc.renderPandoc style)
                 $ formatted
        pandoc = Pandoc nullMeta [BulletList blocks]
    makeItem pandoc

renderPandocReferences :: Item CSL -> Item [Reference] -> Compiler (Item String)
renderPandocReferences csl refs = do
    pd <- readPandocReferences csl refs
    return (writePandoc pd)

referencesFilterContext :: Item CSL -> String -> (Reference -> Bool) -> Context Biblio
referencesFilterContext csl name condition = field name $ \bib -> do
    refs <- readBiblio bib
    refs2 <- makeItem $ filter condition $ itemBody refs
    html <- renderPandocReferences csl refs2
    return (itemBody html)

referencesContext csl =
    referencesFilterContext csl "conferencepapers" isPaperConference `mappend`
    referencesFilterContext csl "journalarticles" isArticleJournal `mappend`
    referencesFilterContext csl "books" isBook
