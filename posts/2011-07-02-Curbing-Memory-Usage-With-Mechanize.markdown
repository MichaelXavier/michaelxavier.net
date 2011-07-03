---
title: Curbing Memory Usage with Mechanize
categories: ruby
---

I've noticed in the past that when using the
[Mechanize](http://mechanize.rubyforge.org) library for some screen scraping
projects I've done that memory usage quickly ballooned out of control.  The
cause of this has since eluded me until today: Mechanize keeps a history of
pages in memory. 

As far as I can tell, the default history limit is unlimited.
Considering that a page consists of mechanize objects for all the links, form
elements, etc on a page as well as a representation of the parsed page in
Nokogiri, this can get costly quick if your style is to reuse the Mechanize
agent.

When configuring the agent, set the maximum history thusly:

~~~~{.ruby}
  agent = Mechanize.new do |a| 
    a.max_history = 1
  end
~~~~

You should choose 1 (or perhaps a higher number if you actually take advantage
of history), because 0 effectively means you cannot access the current page you
want. I hope that helps someone.
