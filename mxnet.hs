{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (id)
import Control.Category (id)
import Control.Arrow ((>>>), (***), arr)
import Data.Monoid (mempty, mconcat)
import Hakyll

main :: IO ()
main = hakyll $ do
  match "css/*" $ do
    route idRoute
    compile compressCssCompiler

  match "images/*" straightCopy

  match "javascripts/*" straightCopy

  match "files/*" straightCopy

  match "templates/*" $ compile templateCompiler

  match "posts/*" $ do
    route $ setExtension ".html"
    compile $ pageCompiler
      >>> arr (renderDateField "date" "%B %e, %Y" "Date unknown")
      >>> arr (copyBodyToField "content")
      >>> applyTemplateCompiler "templates/post.hamlet"
      >>> applyTemplateCompiler "templates/layout.hamlet"
      >>> relativizeUrlsCompiler

  match "posts.html" $ route idRoute
  create "posts.html" $ constA mempty
    >>> arr (setField "title" "Posts")
    >>> setFieldPageList recentFirst "templates/compactpost.hamlet" "posts" "posts/*"
    >>> applyTemplateCompiler "templates/index.hamlet"
    >>> applyTemplateCompiler "templates/layout.hamlet"

  match  "index.html" $ route idRoute
  create "index.html" $ constA mempty
    >>> arr (setField "title" "Home")
    >>> requireAllA "posts/*" (id *** arr (take 3 . reverse . chronological) >>> addPostList)
    >>> applyTemplateCompiler "templates/index.hamlet"
    >>> applyTemplateCompiler "templates/layout.hamlet"
    >>> relativizeUrlsCompiler

  match "rss.xml" $ route idRoute
  create "rss.xml" $ requireAll_ "posts/*" >>> renderRss feedConfiguration

addPostList :: Compiler (Page String, [Page String]) (Page String)
addPostList = setFieldA "posts" $
    arr (reverse . chronological)
        >>> require "templates/postitem.hamlet" (\p t -> map (applyTemplate t) p)
        >>> arr mconcat
        >>> arr pageBody

straightCopy :: RulesM (Pattern CopyFile)
straightCopy = do
  route idRoute
  compile copyFileCompiler

feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration { feedTitle       = "MichaelXavier.net - Infrequently Blogging on Ruby, Haskell, and Other CS stuff.",
                                        feedDescription = "Personal blog of Michael Xavier",
                                        feedAuthorName  = "Michael Xavier",
                                        feedRoot        = "http://www.michaelxavier.net" }
