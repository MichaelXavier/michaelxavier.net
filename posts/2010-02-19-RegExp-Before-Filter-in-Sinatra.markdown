---
title: RegExp Before Filter in Sinatra
categories: sinatra,ruby
---

Sinatra doesn't support arguments to the _before_ filtering method for routing. I wrote a *very* small method to support this.

I suppose the reason why Sinatra's before filter is so simple is because most of the time a Siantra app only does 1 very small thing. My apps in Sinatra have been getting more complex but I don't want to incur all the overhead and boiler plate associated with Rails. RegExp-based filtering is good for keeping your code DRY and adding things like authentication only for specific routes, say ones starting with /admin

Here's what my little extension looks like:

~~~~{.ruby}
module Sinatra

  module RegexpRouteFilter
    def before_with_regexp(pattern, &blk)
      before do
        instance_eval(&blk) if request.path =~ pattern
      end
    end
  end

  register RegexpRouteFilter

end

Here's a sample of how you'd use it in a routes file:

~~~~{.ruby}
before_with_regexp(/^\/admin/) do
  # call authentication methods here
  halt 401, "GO AWAY!"
end

get '/admin/super_secret' do
  #...
end

get '/admin/do_junk' do
  #...
end
~~~~
