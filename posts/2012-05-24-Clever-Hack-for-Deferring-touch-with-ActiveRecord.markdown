---
title: Clever Hack for Deferring touch with ActiveRecord
categories: ruby, rails, metaprogramming
---

## Brief Review of Touch
`touch` is an instance method available on ActiveRecord objects to simply bump a record's `updated_at` or `updated_on` timestamp. This is often useful for child classes to "poke" their parent class when saved in order to keep timestamps up-to-date and run callbacks. Here's an example:

~~~{.ruby}
class Order < ActiveRecord::Base
  has_many :line_items
end

class LineItem < ActiveRecord::Base
  belongs_to :order, :touch => true 
end
~~~

Now, when you update a line item, the order will be saved with an updated
timestamp and all callbacks will be fired as part of that save.

## Why Defer Touch
In very rare edge cases, it may make sense for performance reasons to defer
calls to touch for a time. At work, we have a similar setup with Order and
LineItem. Updating a LineItem touches the order in order to fire some important
callbacks.

However, part of our checkout process involves several sequential saves to line
items. I'm not going to go into the details but imagine something like this:

~~~{.ruby}
def checkout(order)
  order.line_items.each do |line_item|
    line_item.some_operation_that_saves
  end
end
~~~

This means for every line item saved, there will be an UPDATE on order to save
the timestamp. In this case, the complete accuracy of the order timestamp is
not important. It happened that in this specific case, the callbacks on the
order update were fairly expensive and made checkout with large orders far too
slow.


## Beat It With The Metaprogramming Stick
I usually try to use metaprogramming sparingly because I think that it can
quickly make your code unreadable. However, I think there is no getting around
it here. I needed to record all calls to `touch` on specific models in a code
block and then play them back at the end, removing unnecessary duplicate calls.

I needed to institute a strict no-touching policy:

![](/assets/errata/no_touching.jpg "NO TOUCHING!")


~~~{.ruby}
module DeferredTouching
  def self.no_touching!(*klasses, &block)
    to_touch = Set.new
    # 1.8.7 compatibility
    old_methods = klasses.inject({}) {|acc, klass| acc[klass] = klass.instance_method(:touch); acc }

    begin
      klasses.each do |klass|
        klass.send(:define_method, :touch) do
          to_touch << self
        end
      end

      block.call
    ensure
      old_methods.each do |klass, meth|
        klass.send(:define_method, :touch, meth)
      end

      to_touch.each(&:touch)
    end
  end
end
~~~

This works by setting up a unique set of records to be touched in the closure
of the method. It then caches the previously existing touch methods from each
class and redefines them to push the record into the set when touched. Lastly,
it puts the old methods back when the block is done executing and touches each
member of the set.

Here's some example usage

~~~{.ruby}
# Specify only the classes for which to defer touching
DeferredTouching.no_touching!(Order, LineItem) do
  order.line_items.each do |line_item|
    line_item.some_operation_that_saves
    # Just for fun, these touches will get folded into 1 touch
    line_item.touch
    line_item.touch
    line_item.touch
    line_item.touch
    order.touch
  end
end
# order and each line item will be touched exactly once.
~~~

Note a couple of gotchas with this approach: 

* It does not support arguments.  Touch takes an optional column argument, and
  this approach would need some tweaking to support that.
* It operates outside of the transaction. If halfway through the block,
  something blows up, and all updates get rolled back, the ensure will ensure
  that each record that was touched gets touched, regardless. This was not a
  problem for my use case.

## Disclaimer
Realize also that the fact that this is necessary is probably a code smell. If
you find that you must do battle with ActiveRecord like this for performance
reasons, it probably means that you are leaning far too heavily on ActiveRecord
callbacks, and should consider opting for injecting just the right context at
the right time so you aren't running unnecessary database calls in the first
place.

That being said, I was working on a legacy project that already reeks like a
garbage barge. I had a very specific performance problem to address and this
allowed me to resolve it quickly for a massive reduction in response time for
pathologically large orders. If you are in a position to make a more
principaled approach, please do so, but it always helps to have tricks like this
in your back pocket.
