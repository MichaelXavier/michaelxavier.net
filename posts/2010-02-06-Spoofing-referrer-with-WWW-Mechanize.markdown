---
title: Spoofing referrer with WWW::Mechanize
categories: ruby,mechanize,scraping,mechazilla
---

I'm working on a little utility script that scrapes with the wonderful WWW::Mechanize. I found that I needed to use a proper referrer so less of my downloads would stop coming back 403 Forbidden. I'll do a quick code sample to show how I got past this.

So this project I'm working on is called MechaZilla. I'm not planning on releasing it just yet because it's more an exercise in learning the proper structure and code best practices of releasing utilities than the actual functionality (which could be seen in pretty much any download accelerator program). Basically, it allows you to specify a Regexp for either the URL or link text of the link on a page or pages. Example usage would be something like:

~~~~{.sh}
#Grab all links with urls ending in .jpg from 2 sites and save them to /tmp
mechazilla -u '/.jpg$/i' /tmp http://www.example.com http://www.foobar.com
~~~~

Problem is that some sites may be more fine tuned about hotlinking or downloads like this so they would start throwing out 403s. One solution is to throttle the rate at which the files are downloaded (which I will probably implement soon). I figure that the parent directory of some file is likely going to be an acceptable referrer for a download so I added a method in my app that looks something like this:

~~~~{.ruby}
def spoof_referrer(uri)
  up_one = uri.path.to_s.split('/')[0...-1].join('/')
  WWW::Mechanize::Page.new(URI::Generic.build(:scheme => uri.scheme, :host => uri.host, :port => uri.port)).merge(up_one), {'content-type' => 'text/html'})
end
~~~~

This rebuilds a uri but gets the directory 1 up from the file being requested. Note that the content-type param must be passed to instantiate WWW::Mechanize::Page. This part was the hardest to figure out from existing examples so I had to dig through the Mechanize code a bit.

So go forth and spoof your referrers, you screen scraping scoundrels!
