# What is this?

This is the source code to my [blog](http://michaelxavier.net). It is generated
using [hakyll](http://jaspervdj.be/hakyll/), a static site generator written in
Haskell.

To build it, install nix and direnv. Run `direnv allow` it will
automatically provision all tools needed for development. From there,
standard cabal commands will work. For example, you can run `cabal run
mxnet -- watch --host localhost --port 8000` for a development
preview.
