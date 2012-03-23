---
title: Quick Haskell Refactoring with Grep
categories: haskell, linux, refactoring
---

I came up with an idea the other day when listening to [the latest Ruby Rogues podcast](http://rubyrogues.com/046-rr-objects-in-rails-part-2/).
In it, there was a lot of debate about the edition of Enumerable#lazy to Ruby 2.0, which would allow for chained method calls on lists that would build up and then get evaluated lazily to prevent extra trips through the list like so:

~~~{.ruby}
[1,2,3,4].map {|x| x + 3}.select {|x| x > 5}
~~~

The host who aired on the side of object oriented programming prescribed that
these method chains should be extracted into a method which better describes
them for reuse and readability. That's what got me thinking.

In Haskell, instead of method chaining we compose functions with the `.`
operator. As I learn more about Haskell I find myself using composition more
and more to succintly summarize a transform I want to perform to get data from
one type to another. The downside to it being so easy in Haskell is it becomes
easy to introduce redundancies into your code at the cost of expressiveness.

From your project directory, you can run:

~~~{.sh}
$: find . -name '*.hs' | xargs grep -Posh '[\w\.]+ \. [\w+\.]+' | uniq -cd

      2 show . length
      2 maybeRead . T.unpack
~~~

This line of shell script does the following:

1. finds composed functions in your Haskell files
2. Makes the list uniq
3. Displays only those which are duplicated.

That's pretty powerful. A good refactoring would be to find instances of these
compositions and extract them to a better-named function and put them in a Util
module if they are used throughout your application. For example, `maybeRead .
T.unpack` could be more readable as tryReadText. The regex isn't quite right
for multiple compositions strung together but I don't quite want to work that
out for the sake of example.

I find as I'm learning to improve my Haskell-fu that while we can write very
terse programs in Haskell, a well-named function will almost always be
preferable to long chains of function composition in an already-busy function
definition. OO and FP programmers would do well to strive for compact,
expressive methods/functions. I used to think that I should only split up long
functions when I knew I was going to reuse some component of the function. I
now realize that I should be much more liberal with decomposing long functions
so the reader can see a high level, grokable definition and to look at the
definitions of the components if further explanation is necessary.
