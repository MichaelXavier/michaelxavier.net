---
title: "PROTIP: Update Nokogiri When Updating WWW::Mechanize"
categories: ruby,mechanize,scraping
---

As I found out the hard way, you should really update Nokogiri when updating WWW::Mechanize (which is now at 1.0.0).

I noticed the other day that I hadn't updated WWW::Mechanize in a while so I updated it to 1.0.0. There was a small quirk with the WWW module being deprecated. Mechanize is the new toplevel so any references to WWW::Mechanize should now just be Mechanize. Nothing a quick find and replace couldn't fix.

What wasn't so simple was this issue:

    Fatal error: undefined method `<=>' for <input type="hidden" name="loginType" value="L">:Nokogiri::XML::Element
    /usr/lib/ruby/gems/1.8/gems/mechanize-1.0.0/lib/mechanize/form/field.rb:30:in `<=>'
    /usr/lib/ruby/gems/1.8/gems/mechanize-1.0.0/lib/mechanize/form.rb:171:in `sort'
    /usr/lib/ruby/gems/1.8/gems/mechanize-1.0.0/lib/mechanize/form.rb:171:in `build_query'
    /usr/lib/ruby/gems/1.8/gems/mechanize-1.0.0/lib/mechanize/form.rb:221:in `request_data'
    /usr/lib/ruby/gems/1.8/gems/mechanize-1.0.0/lib/mechanize.rb:452:in `post_form'
    /usr/lib/ruby/gems/1.8/gems/mechanize-1.0.0/lib/mechanize.rb:370:in `submit'

Puzzled me for a bit. Turns out you need to update Nokogiri to the latest as it is relying on the spaceship operator being implemented on Nokogiri::XML::Element.
