---
title: Announcing the Release of Web.Campfire
categories: haskell,campfire
---

I'm very proud to the release of my first Hackage project to HackageDB:
Web.Campfire! For being a rather simple library, it took me quite some time to
learn how to properly set up a library that wraps a web API, document it, and
release it. You can check it out on
[hackage](http://hackage.haskell.org/package/campfire) or
[github](http://github.com/MichaelXavier/Campfire).  So if you need to write a
Campfire bot in Haskell or something, consider Web.Campfire.

## Usage
  
Web.Campfire runs in a monad, or particularly, inside of a ReaderT from
Control.Monad.Reader. Below is an example program using the library:

    #!sh_haskell
    {-# LANGUAGE OverloadedStrings #-}
    import Web.Campfire
    import Web.Campfire.Monad
    import Web.Campfire.Types
    import Control.Monad.Reader
    import Data.Text (unpack)

    doStuff :: CampfireM ()
    doStuff = do
      (room:_) <- getRooms
      let rid = roomId room
      joinRoom rid
      speak rid stmt
      leaveRoom rid
      return ()
              where stmt = TextStatement { statementBody = "ATTENTION: I have nothing important to say" }

    main :: IO ()
    main = do
      runReaderT (unCampfireM doStuff) env
      me <- runReaderT (unCampfireM getMe) env
      putStrLn "Hello, my name is:"
      putStrLn . unpack $ userName me
          where env  = CampfireEnv { cfKey = "MYKEY", cfSubDomain = "mysubdomain"}

## http-enumerator > Network.Curl

I had gone through several HTTP libraries before arriving on the excellent
[http-enumerator](http://hackage.haskell.org/package/http-enumerator) by
[Michael Snoyman](http://www.snoyman.com). Campfire's API requires SSL,
authenticates with HTTP Basic (using the user token as the login and an
empty/blank password), and uses PUT and DELETE on some calls.

This unfortunate confluence of requirements caused me to implement the HTTP
backend of Web.Campfire several times:

1. Network.HTTP doesn't support HTTPS. You can indicate that the URL is secure and use port 443 but at its core, Network.HTTP will just use plain HTTP. Strike one.
2. http-enumerator's API was simple and lovely but it did not support HTTP Basic Auth. I didn't even notice this until I needed it and had implemented everything else. Strike two. 
3. Network.Curl has a horrendous API, especially for PUT and DELETE. I was only able to get GET and POST requests working with Network.Curl after beating it with a stick for days and enlisting the help of the haskell-beginners mailing list. I'm assuming the underlying C API is the culpret behind how ugly using Curl gets, but the huge jump in complexity going from GET to POST requests made me desperate for an alternative. Strike three.
4. Someone I talked to on the mailing lists implemented HTTP Basic and all was well with the world.

The suggestion I got on the mailing list for a POST in Curl looked like:

    #!sh_haskell
    {-# LANGUAGE ScopedTypeVariables #-}

    import Control.Exception (IOException, handle)
    import Control.Monad (liftM)
    import qualified Data.ByteString as BSS
    import qualified Data.ByteString.Lazy as BS
    import qualified Data.ByteString.Lazy.Char8 as BS8
    import Data.IORef
    import qualified Network.Curl as Curl
    import Network.URI (URI)


    post :: URI -> BS.ByteString -> String -> IO (Maybe BS.ByteString)
    post uri body contentType = handleIOException (const $ return Nothing) $ Curl.withCurlDo $ do
          bodyRef <- newIORef []
          h <- Curl.initialize
          mapM_ (Curl.setopt h) $ [Curl.CurlURL $ show uri,
                                    Curl.CurlNoBody False,
                                    Curl.CurlFollowLocation False,
                                    Curl.CurlMaxRedirs 0,
                                    Curl.CurlAutoReferer False,
                                    Curl.CurlUserAgent "Mozilla/5.0",
                                    Curl.CurlNoSignal True,
                                    Curl.CurlPostFields [BS8.unpack body],
                                    Curl.CurlHttpHeaders ["Content-Type: " ++ contentType],
                                    Curl.CurlWriteFunction $ bodyFunction bodyRef]
          code <- Curl.perform h
          if code /= Curl.CurlOK
              then return Nothing
              else liftM (Just . BS.fromChunks . reverse) $ readIORef bodyRef

    bodyFunction :: IORef [BSS.ByteString] -> Curl.WriteFunction
    bodyFunction r = Curl.gatherOutput_ $ \s -> do
                      bs <- BSS.packCStringLen s
                      modifyIORef r (bs:)

    handleIOException :: (IOException -> IO a) -> IO a -> IO a
    handleIOException handler action = handle (\(e :: IOException) -> handler e) action

Dreadfully crufty. My actual implementation with http-enumerator is loads simpler. The majority of the body of the http-enumerator implementation is just setting up the request in a declarative fashion, as it should be.

## Project Notes
While I do think I did a good job on documentation, project layout, formal
testing on this project is zero. If I get some time set aside for it in the
future, I'll set up some formal tests. When I was working on this project, I
was testing all the functionality by hand, so I'm fairly confident everything I
have implemented thus far works.

There are 2 parts of the published Campfire API that Web.Campfire does not yet
cover. The first is the streaming API. This is a very powerful feature and I
would love to implement it in the future. This API allows you to keep a
connection open and receive new messages in JSON as they come in.

The other unimplemented part is file uploads. This one seems like it would be
much easier to implement than streaming.