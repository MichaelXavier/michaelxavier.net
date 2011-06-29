---
title: Leave Object Alone
categories: mongoid,mongodb,antipattern,ruby
---

I love programming. This can be a very advantageous disposition to have considering it is my job to write code. However, when something stupid comes up and prevents me from getting stuff done, I become *VERY* uneasy. This time it was a bad Ruby citizen monkeying around with Object.

[Mongoid](http://mongoid.org) is a pretty nice project. I am using
[MongoDB](http://mongodb.org) in a metrics project I'm doing for my internship
and switched from [MongoMapper](http://mongomapper.com) after I got fed up with
the syntax and some bugs I was experiencing. Mongoid is without a doubt a nicer
to use library, but is not without its faults. This time, I was trying to set
up my application to deploy with [Vlad the
Deployer](http://rubyhitsquad.com/Vlad_the_Deployer.html). Like capistrano,
Vlad uses the "set" method all over the place to configure options. However,
     unlike Capistrano, it uses Rake directly, rather than in some weird,
     rake-like Capfile. And so the trouble began...

For some reason, rake was expecting "set" to be called with one argument, not
two. This made absolutely no sense given that set was used for setting
configuration keys, which requires the key and value. I tried differen versions
of Vlad, Vlad-Git. Enraged, I created a separate branch and started trying to
get the damn thing working with Capistrano.

Cut to a couple hours later. I've turned off all the lights in the room. I'm
leaving horrible obscenities in the source code of rspec for suppressing my
calls to ruby-debug. I'm intentionally calling set with incorrect arguments in
order to trick ruby into letting me trace into whatever had inflicted this hell
upon me. Behold:

~~~~{.ruby}
module Mongoid #:nodoc:
  module Extensions #:nodoc:
    module Object #:nodoc:
      # This module converts objects into mongoid related objects.
      module Conversions #:nodoc:
        def self.included(base)
          base.class_eval do
            include InstanceMethods
            extend ClassMethods
          end
        end

        #...

        module ClassMethods
          # LOL I TROL U
          def set(value)
            value.respond_to?(:attributes) ? value.attributes : value
          end

          #...
        end
      end
    end
  end
end
~~~~

WHAT?
-----

![My face when Object was extended](http://img837.imageshack.us/img837/8731/plfrontier1.png)

Mongoid, how could you? I thought we were best bros...

The last insult of course was that after literally hours of hacking around trying to figure this out, the solution that worked for me was to move my lines requiring vlad _up 2 or 3 lines above where the environment is loaded_. I love the freedom that Ruby provides us but sometimes I feel like Matz just gave a bunch of kids loaded guns to play with.
