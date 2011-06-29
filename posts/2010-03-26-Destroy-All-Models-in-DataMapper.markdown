---
title: Destroy All Models in DataMapper
categories: ruby,datamapper
---

I found when writing a spec helper recently that I should have been destroying all models before each test like Rails would.

Thanks to the awesome [DKubb](http://github.com/dkubb) from #datamapper on FreeNode for this tip that should have been obvious to me. In my spec/spec_helpers.rb file, I have added the following configuration option:

    #!sh_ruby
    Spec::Runner.configure do |config|
      config.before(:all) do
        DataMapper::Model.descendants.each {|m| m.all.destroy }
      end
    end

Make *sure* that you are working on your _test database of course. You may want to use :each instead of :all depending on your tests. Remember that :each is run before each example, :all for each example *group*.