---
title: find_in_batches And the Query Cache
categories: rails, ruby
---

I should preface this post by saying that may only be particular to Rails 2.3.X. Unfortunately, all of the major projects at work use Rails 2 so I haven't had much work with large datasets using the latest version of ActiveRecord

## Use Case for find_in_batches and find_each

We use find_in_batches and find_each a lot at work for oneoff scripts and large scale data transforms. A good example may be modifying several thousand products in the database. If you were to simply execute a `Product.all.each` could very easily bring your production server to its knees and make you feel quite foolish. Instead, you would do something like:

~~~~{.ruby}
  Product.some_scope.find_each do |product|
    # do stuff with each product
  end
~~~~

You can read (the documentation)[http://railsapi.com/doc/rails-v2.3.8/classes/ActiveRecord/Batches/ClassMethods.html#M001252] for the details, but the gist is that Rails will handle the pagination on the backend, fetching 1000 records at a time. 

find_in_batches is the generic case for this and will yield in each batch rather than each product in the batch.

These methods allow you to work with a large number of records without having to load all of them into memory at once. Production servers do not enjoy swapping.

## Unreasonable Default?
Some time ago I had learned a lesson that I recently forgot. ActiveRecord in 2.3.X implements a query cache which is pretty much a Hash keyed on the SQL. This behavior is *ideal* for handling web requests. It is reasonable behavior in the context of a single web request to produce the same data when given the same query. If you load some data in the beginning of handling a request, for most applications it is perfectly sane (and desirable) to just cache that result for the duration of the request in case the same query is made.

However, most use cases for find_in_batches seem to be for maintenance and administation scripts that do not coincide with a user request. It was a bit of a surpise to me to find out that the query cache is still used in find_in_batches and find_each. Because of the paginating that these methods do, you will most likely not execute the same query twice unless you do the same find_each or find_in_batches in the same request/script. Also, because the need for these methods is predicated on trying to prevent large datasets from exhausting memory, if any methods should skip the ActiveRecord query cache it would be these two.

## Solution

ActiveRecord::Base has an instance method called uncached which takes a block. ANy code executed in this block will do so without writing to or reading from the query cache. You could use it like:

~~~~{.ruby}
  Product.uncached do
    Product.find_each do |product|
      # do stuff with each product
    end
  end
~~~~

I'm not sure how I feel about Rails omitting this optimization. On one hand, I have a hard time imaginging a case where you'd use either one of these two methods where the query cache would do anything but eat up memory for no reason. On the other hand, the Rails guys could easily argue that it is not Rails' business how you use find_in_batches and therefor it should not try to optimize the code for you. That is a certainly defensible stance. A mention of being wary of the query cache when using this method would probably satisfy everyone.
