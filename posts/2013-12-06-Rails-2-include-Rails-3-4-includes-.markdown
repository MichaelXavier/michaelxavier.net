---
title: Rails 2 :include != Rails 3/4 includes()
categories: 
---

Not too long ago at work we made the big painful switch of our largest legacy
apps from Rails 2 to Rails 3. While there is no shortage of documentation on
the pitfalls one may face when doing this, I wanted to bring up one that was
particularly insidious.

Some may remember that in Rails 2, eager loading was typically done by passing
a `:include` option to a finder, specifying the graph of records you'd like to
eager load, e.g. `Post.all(:include => [:author, {:comment => :commenter}])`.
In most cases, this would issue a query to each table involved, first finding
`authors` whose `id` corresponded to the `author_id` on the post, then
comments, and so-on. The important thing to note is that it would not do a
join.

When upgrading and converting from options-in-finder style to the class method
chaining style of ActiveRecord 3, it seemed obvious that `.includes()` was
analogous to `:include`. It is not.

As this [wonderful article](http://blog.bigbinary.com/2013/07/01/preload-vs-eager-load-vs-joins-vs-includes.html)
points out, there are actually 4 related methods in rails to eager load:
`preload`, `includes`, `eager_load`, and `joins`. `includes` will do separate
queries **unless** your query is specifying tables and columns explicitly in
it, in which case **it uses a left outer join**. I believe I read in a Rails github issue
which I have now lost track of that in Rails 3, this behavior scans your
conditions for a '.' character, even if it is against your primary table. We
have to be explicit in some of our scopes because they may or may not be
used while joining with other tables that have the same column names.

## tl;dr
`preload` always does separate queries, `joins`
always does *inner* joins, and `eager_load` always does outer joins. Nothing
against the Rails team but the tradeoffs and semantics inherant in these
different strategies are in no way communicated via their method names. They
sound like aliases.

In our particular case, our wholesale conversion from `:include` to
`includes()`, especially when applied to rather lengthy scope chains via search
caused a huge hit in query performance. Many of these cases could be returned
to their previous speed by preferring to use `preload` instead of `includes`.
This will obviously vary case by case but if your usage of ActiveRecord scopes
is not well optimized for left outer joins (which can have pretty poor
performance characteristics), be aware of your options and note that while it
seems like `includes()` is equivalent to `:include`, that is not quite the
case.
