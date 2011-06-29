---
title: Watch Your self Around DataMapper
categories: datamapper,tip
---

Always keep in mind the order in which your methods are defined when using a DSL to define a class.

I had a bit of an issue the other day that unfortunately resulted in about an hour or more of lost time trying to figure this issue out. I had 2 models with DataMapper which looked something like:

~~~~{.ruby}
class Client
  include DataMapper::Resource

  class << self
    def default_repository_name
      :hive
    end
  end

  property :id,   Serial, :writer => :private
  property :name, String, :writer => :private
end

class Product
  include DataMapper::Resource

  property :id,   Serial, :writer => :private
  property :name, String, :writer => :private

  class << self
    def default_repository_name
      :hive
    end
  end
end
~~~~

The default_repository_name was used because I am using these models to map a legacy datastore and the repository needs to be switched on the fly. There was a lot more surrounding code and I could not for the life of me figure out why Client was functioning normally and Product was acting like it didn't have any defined properties. It turns out that methods like default_repository_name **should be put near the top of the model**. When you start making calls to the DataMapper DSL to define properties or using plugins, they will be called within the context that you have defined thus far in the class. In short:

#### _Order Matters_

I've been doing a lot of interesting stuff lately with DataMapper so expect some more posts on that topic in the near future.
