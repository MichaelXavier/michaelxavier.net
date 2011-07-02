---
title: I Don't Understand The New Hash Syntax
categories: ruby
---

This may come across as yet another blog post complaining about the new Ruby
1.9. It is. I just want to throw my hat in the ring and say that the tradeoffs
are not worth it.

## Disclaimer

I know that posts like this aren't very valuable. What dissenters say isn't
likely to revert this change. Moreover, it is an additive change to Hash, so
curmudgeons like me can continue using the old syntax. I liken this a lot to
the furor that erupted over the addition of Coffeescript as part of the Rails
default gemset. You don't have to use it, but the affordance it provides can
cause some problems for us, which I will enumerate later.

## The New Syntax

In brief, the new Ruby Hash syntax looks like:

~~~~{.ruby}
  {key_here: 'value here', another_key:'another value'}
~~~~

This creates a hash with symbol keys which would be created like this in Ruby 1.8:

~~~~{.ruby}
  {:key_here => 'value here', :another_key => 'another value'}
~~~~

## Arguments Against

### Hash is an Associative Array

A hash in Ruby is an [associative
array](http://en.wikipedia.org/wiki/Associative_array). In short, it is a data
structure where keys are mapped to values. Being a loosely-typed programming
language, Ruby allows you to use whatever you want as keys, and keys within a
hash can even be heterogenous. The advantage of the good old hash rocket syntax
is that it communicates this concept precicely.

~~~~{.ruby} 
  prices = {'product 1' => 3.50, 'product 2' => 10.00}
  regions = {1 => ['joe', 'bob'], 2 => ['Mary', 'sally']}
  options = {:verbose => true}
~~~~

In the above code, it is immediately evident that prices maps a product to a
price and that regions maps some sort of region number to a list of names and
options maps symbols to values. There are many places in code I've worked on
where arrays of differing key types must comingle. If I had to construct these
same hashes next to eachother, I'd feel silly writing this:

~~~~{.ruby}
  prices = {'product 1' => 3.50, 'product 2' => 10.00}
  regions = {1 => ['joe', 'bob'], 2 => ['mary', 'sally']}
  options = {verbose:  true}
~~~~

I'd feel silly writing it because the first two hashes maintain transparancy
with respect to what data they contain. You are expressing an associative array
in a manner which _obfuscates the central feature of associative arrays_. 

Options actually maps symbols to values, but usess an entirely different
key/value separator to express the same data type. If I were to look at the
latter hash syntax as a mapping of keys to values coming from another language,
I'd almost assume that verbose was an unquoted token or variable. I could also
assume that it is similar to an _atom_ in Erlang (and I'd be right, because
atoms are analogous to Symbols), but would be a bit confused to find that
symbols as literals are expressed with a : _first_, not second. 

I know this ambiguity doesn't get to us as l337, bleeding-edge following
Rubyists, but at some point, we must weigh the benefits of reeling in the
number of ways you can accomplish the same thing. Doing so will make it easy on
newcomers and veterans alike by reducing the number of Ruby dialects that can
be seen in the wile.

### High Cost, Low Return

I hate being this guy but the new syntax does break backwards compatibility of
hashes when used. I think it is a fallacious argument to stop there. Breaking
backwards compatibility is not always a bad thing. Indeed it is critical to
progressing the language. I do *not* want Ruby to be held back and stifled by
it's own success. A version change from 1.8 to 1.9 by virtue of [semantic
versioning](http://semver.org) may provide new functionality given there is
backwards compatibility if the user wants similar functionality from the old
verison. However, when making a breaking change like this, language designers
should be sure that the benefits of the move will outweigh the inconvenience
of:

1. Learning the new syntax.
2. Converting old code to new code.
3. Bringing in the new standard into a group project's coding standard if
   applicable.

I do not see the benefits of the new Hash syntax meeting these criteria.

I work on a few projects at my day job that are what we consider _hopelessly_
Ruby 1.8. The performance gains of REE are such that sticking to 1.8 isn't so
painful, but it is simply too costly to upgrade to 1.9. That's all fine and
well. The problem is if someone designing the next must-have Rails plugin or
gem decides that the new syntax is the greatest thing in the world, I _cannot_
use their code without forking and vendoring the code because 1.8 will not even
_parse_ the code, to which the new syntax contributes nothing.

### But Its Like JSON

It bums me out when I hear this argument. JSON is a fantastic data
serialization format. I, like most modern Rubyists, huff and puff when I find
an API that still only trafficks in XML or god forbid SOAP. JSON is simple,
compact and elegant. That being said, _Ruby is not Rails_. Ruby is a
general-purpose programming language. It is not a DSL for processing JSON. Ruby
is used for a lot more than web applications. I don't understand the premise
behind this argument when I hear it.

If the premise of this argument is that designers and people coming from a
JavaScript background can jump into Ruby with greater ease, then it is an
extremely weak one. A fledgling Ruby programmer will encounter far more jarring
things than figuring out _"Oh, ok, so you use => to separate keys from values
instead of:"_. Lambdas, blocks, and the cavalcade of other things that make
Ruby a powerful, introspective language would still be stumbling blocks to
programmers accustomed to JSON. We do not need to trick anyone into learning
Ruby by taking on the trappings of their language of choice.

### Conclusion
I'll be fine with this new Hash syntax. Sadly, I rarely get to code in Ruby
1.9. When I do eventually work on a 1.9 project with others using the new
syntax , I am left with 3 options, all of them bad:

1. Go with the flow, knowing I'm shutting out people stuck in 1.8 from my code
   for something that doesn't buy me a damn thing.
2. Be that douchebag that submits a pull request that, along with my own
   contributions, changes all of the hashes to use hash rockets. Nobody will
   take that pull request.
3. Use hash rocket syntax in just my own code contributions, making for
   inconsistent-looking code that does not communicate a decided-upon coding
   standard.

I don't want to choose any of those. The harm in the new syntax is that if I
want to code in 1.9 and if people adopt it, I cannot help but make that
dreadful choice.
