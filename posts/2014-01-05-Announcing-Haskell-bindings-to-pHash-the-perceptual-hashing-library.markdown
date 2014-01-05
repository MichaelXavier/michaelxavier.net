---
title: Announcing Haskell bindings to pHash, the perceptual hashing library
categories: haskell, projects
---

Just a quick announcement. This weekend I did my first FFI project in any
language. It was a learning experience. I initially tried to distribute the C
code for pHash but oh my god do things get ugly that way. So you'll need to
have the phash shared library installed for this to work.

pHash is a perceptual hashing library, which you can use to identify images
that are similar. It can also be used to hash audio and video files but I had
trouble getting the bindings working there and most people seem to use it
mostly for images. Check it out on
[HackageDB](http://hackage.haskell.org/package/phash) or grab the source on
[Github](http://github.com/MichaelXavier/phash).
