---
title: Deep Copy Params Hash When working with Rack
categories: rack,ruby,middleware
---

I'm not sure why this happened. I think I remember something about Rack::Request being a singleton or something. You should deep copy anything you're going to modify from your rack env in middleware.

One of my projects for my internship is to create a data mining tool that logs
requests to a Riak datastore. For me this involves a pretty simple Rack
middleware pipeline. The other day I got it in my head that I should probably
not be logging passwords in the raw. I threw together a really simple,
configurable password filter for my middleware stack that changes
parameters to [FILTERED] just like rails does. I spent the better part of
the morning trying to figure out why my selenium-driven tests kept losing
my session.

It turns out that parsing the request with Rack::Request.new as the culpret. I
don't remember exactly how that is implemented but it must use a singleton or
something. It turns out that if you assign your request's params to something
else and then start modifying that value _it will modify the request params
before they get to your app_. Yeah, that's bad. My middleware looks something
like

    #!sh_ruby
    class PasswordFilter < Middleware::Generic
      def call(env)
        req = Rack::Request.new(env)
        env['request_logger.log_entry'][:params] = req.params # set up the env key for the rest of the pipeline
        @hide_params.each {|param| set_hash_path(env['request_logger.log_entry'][:params], param)}
        super(env)
      end
      #...
    end

Evidently my modification of my copy of params affected the request so
poor selenium was doing its job and Rack was submitting every login with the
password [FILTERED]. Much to my surprise, #dup and #clone did nothing to solve
the problem. I ultimately had to use Marshal.load(Marshal.dump(the_hash)) to
get this working. Hopefully I can prevent someone from losing an hour or two to
such a stupid issue.