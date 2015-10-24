--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative((<$>))
import           Data.Monoid ((<>))
import           Hakyll

--------------------------------------------------------------------------------
main :: IO ()
main = hakyllWith cfg $ do
    match "images/*" straightCopy
    match "assets/**/*" straightCopy
    match "javascripts/*" straightCopy
    match "files/*" straightCopy

    create ["rss.xml"] $ do
      route idRoute
      compile $ do
        let feedCtx = postCtx <> bodyField "description"
        posts <- recentFirst =<< loadAllSnapshots "posts/*" "content"
        renderRss feedConf feedCtx posts

    match "css/*" $ do
      route   idRoute
      compile compressCssCompiler

    match "posts/*" $ do
      route $ setExtension "html"
      compile $ pandocCompiler
        >>= saveSnapshot "content"
        >>= loadAndApplyTemplate "templates/post.html"   postCtx
        >>= loadAndApplyTemplate "templates/layout.html" postCtx
        >>= relativizeUrls

    match "pages/resume.markdown" $ do
      route $ setExtension "html"
      compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/page.html"   postCtx
        >>= loadAndApplyTemplate "templates/minimal_layout.html" postCtx
        >>= relativizeUrls

    match "pages/*" $ do
      route $ setExtension "html"
      compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/page.html"   postCtx
        >>= loadAndApplyTemplate "templates/layout.html" postCtx
        >>= relativizeUrls

    -- create ["pages/resume-min.html"] $ do
    --   route (setExtension "html") "pages/resume.html"
    --   compile $ pandocCompiler
    --     >>= loadAndApplyTemplate "templates/page.html"   postCtx
    --     >>= relativizeUrls

    create ["posts.html"] $ do
      route idRoute
      compile $ do
        posts <- recentFirst =<< loadAll "posts/*"
        let archiveCtx = listField "posts" postCtx (return posts) <>
                         constField "title" "Posts"               <>
                         defaultContext
        makeItem ""
            >>= loadAndApplyTemplate "templates/posts.html" archiveCtx
            >>= loadAndApplyTemplate "templates/layout.html" archiveCtx
            >>= relativizeUrls

    match "index.html" $ do
      route idRoute
      compile $ do
        posts <- fmap (take postsOnPage) . recentFirst =<< loadAllSnapshots "posts/*" "content"

        let indexCtx = listField "posts" postCtx (return posts) <>
                       constField "title" "Home"                <>
                       defaultContext

        getResourceBody
          >>= applyAsTemplate indexCtx
          >>= loadAndApplyTemplate "templates/layout.html" indexCtx
          >>= relativizeUrls

    match "templates/*" $ compile templateCompiler


--------------------------------------------------------------------------------
postsOnPage :: Int
postsOnPage = 5


-------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    field "postbody" (return . itemBody) <>
    defaultContext


-------------------------------------------------------------------------------
straightCopy :: Rules ()
straightCopy = do
  route idRoute
  compile copyFileCompiler


-------------------------------------------------------------------------------
feedConf :: FeedConfiguration
feedConf = FeedConfiguration
    { feedTitle       = "michaelxavier.net Blog"
    , feedDescription = "The development blog of Michael Xavier"
    , feedAuthorName  = "Michael Xavier"
    , feedAuthorEmail = "michael@michaelxavier.net"
    , feedRoot        = "http://michaelxavier.net"
    }


-------------------------------------------------------------------------------
cfg :: Configuration
cfg = defaultConfiguration { deployCommand = "cd _site && find * -type f -print | s3funnel michaelxavier.net PUT -t 4 --put-full-path -v"}
