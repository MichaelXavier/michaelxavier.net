--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative((<$>))
import           Data.Monoid ((<>))
import           Hakyll
import           System.Cmd (system)
import           System.FilePath (replaceExtension, takeDirectory)
import           Text.Pandoc as Pandoc

--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
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

    match "pages/*" $ do
      route $ setExtension "html"
      compile $ pandocCompiler
        >>= loadAndApplyTemplate "templates/page.html"   postCtx
        >>= loadAndApplyTemplate "templates/layout.html" postCtx
        >>= relativizeUrls

    -- match "pages/resume.markdown" $ version "pdf" $ do
    --   route   $ setExtension ".pdf"
    --   compile $ do
    --       resumeTpl <- loadBody "templates/resume.tex"
    --       getResourceBody
    --           >>= (return . readPandoc)
    --           >>= (return . fmap (Pandoc.writeLaTeX Pandoc.def))
    --           >>= applyTemplate resumeTpl defaultContext
    --           >>= pdflatex

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

postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    field "postbody" (return . itemBody) <>
    defaultContext

straightCopy :: Rules ()
straightCopy = do
  route idRoute
  compile copyFileCompiler

feedConf :: FeedConfiguration
feedConf = FeedConfiguration
    { feedTitle       = "michaelxavier.net Blog"
    , feedDescription = "The development blog of Michael Xavier"
    , feedAuthorName  = "Michael Xavier"
    , feedAuthorEmail = "michael@michaelxavier.net"
    , feedRoot        = "http://michaelxavier.net"
    }

-- | Hacky.
pdflatex :: Item String -> Compiler (Item TmpFile)
pdflatex item = do
    TmpFile texPath <- newTmpFile "pdflatex.tex"
    let tmpDir  = takeDirectory texPath
        pdfPath = replaceExtension texPath "pdf"

    unsafeCompiler $ do
        writeFile texPath $ itemBody item
        _ <- system $ unwords ["pdflatex",
            "-output-directory", tmpDir, texPath, ">/dev/null", "2>&1"]
        return ()

    makeItem $ TmpFile pdfPath
