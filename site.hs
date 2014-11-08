--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend, mconcat)
import           Hakyll

import Publications

myTransportConfiguration :: Configuration
myTransportConfiguration = defaultConfiguration
    { deployCommand   = "rsync --checksum -ave 'ssh' _site/* aidan@www.phoric.eu:~/www"
    }

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Ontology Engineering with Diagrams"
    , feedDescription = "Technical posts regarding ontology engineering with diagrams and WebProtégé"
    , feedAuthorName  = "Aidan Delaney"
    , feedAuthorEmail = "aidan@phoric.eu"
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

    create ["publications.html"] $ do
      route idRoute
      compile $ do
        books <- getCiteCompiler "/home/aidan/SparkleShare/Proposals/books.bib"
        journals <- getCiteCompiler "/home/aidan/SparkleShare/Proposals/journals.bib"
        confs <- getCiteCompiler "/home/aidan/SparkleShare/Proposals/conferences.bib"
        let pubCtx =
              listField "books" defaultContext (return books) `mappend`
              listField "journals" defaultContext (return journals) `mappend`
              listField "confs" defaultContext (return confs) `mappend`
              defaultContext
        makeItem ""
          >>= loadAndApplyTemplate "templates/publications.html" pubCtx
          >>= loadAndApplyTemplate "templates/default.html" pubCtx
          >>= relativizeUrls

    match "index.html" $ do
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

    -- Render RSS feed (from Eric)
    create ["rss.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx `mappend` bodyField "description"
            posts   <- recentFirst =<<  loadAllSnapshots "posts/*" "content"
            renderRss myFeedConfiguration feedCtx posts

--        loadAllSnapshots "posts/*" "content"
--        >>= fmap (take 10) . recentFirst
--        >>= renderAtom (myFeedConfiguration "All posts") feedContext

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext
