---
title: Temporary Files for Testing in Haskell
categories: haskell, testing, hspec
---

Just a quick tip as I had to try a couple different ways to get it working.

First off, you'll need to install the
[temporary](http://hackage.haskell.org/package/temporary) package.

I needed to test parsing an actual file on disk. I still find some difficulty
writing good tests that perform IO in Haskell, but this worked well enough.

[Buster](https://github.com/MichaelXavier/Buster) has a function to parse a
YAML config file that looks like:

```{.haskell}
loadConfig :: FilePath -> IO (Either String Config)
loadConfig = decodeFileEither

decodeFileEither :: FromJSON a => FilePath -> IO (Either String a)
decodeFileEither fp = decodeHelper (Y.decodeFile fp) >>= either throwIO return
```

I've already thoroughly tested the pure parsing component but I was not
completely certain of my implementation of the file one.

## Wrapping Specs with Temp Files

Here's an excerpt from the test suite:

```{.haskell}
spec :: Spec
spec = describe "parsing from file" $
  it "parses a full config successfully" $
    withPreloadedFile fullConfigStr $ \path ->
      loadConfig path `shouldReturn` Right fullConfig
  where fullConfigStr = "....."
        fullConfig = Config { ... }

withPreloadedFile :: ByteString -> (FilePath -> IO a) -> IO a
withPreloadedFile content action = withSystemTempFile filenameTemplate callback
  where filenameTemplate     = "buster_fixture.yml"
        callback path handle = BS.hPut handle content >> hFlush handle >> action path
```

`withSystemTempFile` will open up a file in your system-specific temporary dir
(such as /tmp) using  the filename template you specify. That file basically
gets treated like a prefix, so it may generate a file like
buster_fixter.ymlAAA, or something close to it.

Take special note of the flush. You want to ensure the write gets flushed
*before* the test runs or it will appear to be empty.
