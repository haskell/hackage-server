--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList []) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
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

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= saveSnapshot "content"
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler

    create ["rss.xml"] $ do
            route idRoute
            compile $ do
                loadAllSnapshots "posts/*" "content"
                    >>= fmap (take 10) . recentFirst
                    >>= renderRss myFeedConfiguration feedCtx

--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

feedCtx :: Context String
feedCtx = mconcat
    [ bodyField "description"
    , dateField "date" "%B %e, %Y"
    , defaultContext
    ]

myFeedConfiguration :: FeedConfiguration
myFeedConfiguration = FeedConfiguration
    { feedTitle       = "Hackage Blog"
    , feedDescription = "Information and News about Hackage and its surrounding infrastructure"
    , feedAuthorName  = "Hackage Trustees/Admins"
    , feedAuthorEmail = ""
    , feedRoot        = "http://blog.hackage.haskell.org"
    }
