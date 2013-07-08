---
title: Rakefile for Developing Cabal Projects in Haskell
categories: development, haskell, ruby
---

I've been struggling with having an adequate Haskell build project for some
time now. I've gone through hosing my cabal db by treating it like Rubygems and
installing every new package. I've tried cabal-dev and found it lacking real
integration with other Haskell tools. I've tried hsenv and had trouble there
too.

What finally worked for me is Cabal-1.17.0, which is currently unreleased. This
has build in sandboxing support which works great. However, to use GHC-provided
tools such as GHCi, you still have to pass flags to point it to your sandboxed
environment.

I'm too fond of my editor (vim) to switch to something like Haskell IDE, as
impressive as it is. I just want a build process that works, and while the
community settles on that, I've set up a gist of a `Rakefile` for common build
tasks, and a `Guardfile` which you can use with Ruby's `guard` to run your
tests automatically while editing. It even sends notifications to your OS'
notification system. It has worked out really well and runs single-module test
suites fast enough that I don't find myself getting slowed down waiting on test
builds too often.

## Setup
The whole thing is available in a
[gist](https://gist.github.com/MichaelXavier/5847963). I will be keeping that
up to date if I have to make any changes. You will need ruby and bundler to run
it. A process to bring it into an existing project should be like:

~~~{.bash}
cd my-project-dir/
wget -qO- https://gist.github.com/MichaelXavier/5847963/download | tar xz --strip 1
bundle # gem install bundler if you don't have it
rake --tasks # list build tasks
guard start # monitor code and run tests
~~~

## Install Notes
I make the following assumption with these tools:

1. You have your project split into `src/` and `test/` dirs.
2. You're using hspec and follow the hspec-discover pattern of having a file
`test/Spec.hs` and that your `test` dir mirrors your `src` files, but with
`Spec.hs` at the end. For example: `src/Foo/Bar.hs` test file will be
`test/Foo/BarSpec.hs`. 
3. You have a recent ruby installed and bundler. 
4. You're using either the newest `cabal` with sandboxing or `cabal-dev`,
preferring the former. `rake sandbox` for example will try to set up a native
sandbox with cabal.
5. Your system supports `nproc` to determine how many cores to build on. Works
on linux. Take it out if that's not cool with you.

Running `rake` will build your package. The `build` task installs dependencies to
your sandbox.  Likewise, the `test` task installs test dependencies.


## Source

<script src="https://gist.github.com/MichaelXavier/5847963.js"></script>
