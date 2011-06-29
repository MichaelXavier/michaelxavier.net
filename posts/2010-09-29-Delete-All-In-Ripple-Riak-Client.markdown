---
title: Delete All In Ripple/Riak-Client
categories: Ruby,Riak,Ripple
---

[Riak](https://wiki.basho.com/display/RIAK/Riak) is a distributed key/value
store. It is really cool. Unfortunately, shy of deleting the data file
entirely, clearing out a bucket is dog slow, poorly documented, and pretty ugly
with the ruby driver.

Sure, you could argue that if you have the need to clear out lots of entries at
once from a Riak KV store, your problem space may not be well suited for a KV
store. Still, it isn't a completely unreasonable thing to do but with
[Ripple](http://github.com/seancribbs/ripple) and the ruby driver, the only way
to clear a bucket is to load all keys into memory and then delete each.
Seriously. That sucks. I've spoken with Sean Cribbs, the gem's author, about
this before and I'm pretty convinced this might be a limitation of Riak rather
than the library. Either way, if you need to delete everything from a bucket,
make sure you understand that it will take a long time and potentially use
a lot of memory.

One great example of when you would want to clear a bucket is as a setup task
for specs:

~~~~{.ruby}
RSpec.configure do |config|
  config.before(:each) do
    Notification.bucket.keys.each {|k| Notification.bucket.delete(k)}
  end
end
~~~~

I hope you like slow test suites!

Edit
----
Turns out there is a subtle but more efficient way to do this. Thanks to [Aphyr](http://aphyr.com)

~~~~{.ruby}
RSpec.configure do |config|
  config.before(:each) do
    Notification.bucket.keys {|keys| keys.each {|k|Notification.bucket.delete(k)}
  end
end
~~~~

Evidently that extra block scoping streams the keys. Neat!
