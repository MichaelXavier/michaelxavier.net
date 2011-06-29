---
title: factory_girl on Rails 3: undefined method config
categories: rails,rails3,factory_girl
---

After upgrading to Rails 3 beta2 I started getting an error (undefined method config for NilClass) when running specs or tests using factory_girl.

This issue with factory_girl is is documented on the factory_girl github [here](http://github.com/thoughtbot/factory_girl/issues#issue/45). The issue occurs with a check at the end of factory_girl.rb. It currently reads "if defined? Rails.configuration" but should read "if defined? Rails.configuration and Rails.application". Unfortunately the thoughtbot team has not gotten around to fixing it so to solve it for now:

Edit your Gemfile thusly: 

~~~~{.ruby}
group(:test) do
  gem 'factory_girl', :git => 'git://github.com/danielb2/factory_girl'
end
~~~~

This will source danielb2's fork which removes all the special setup for rails.

Next, edit your 'spec_helper' and add:

~~~~{.ruby}
require 'factories'
~~~~

The offending file automatically required my spec/factories.rb file so it's no big deal to add this in manually.
