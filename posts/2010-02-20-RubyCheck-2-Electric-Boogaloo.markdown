---
title: RubyCheck 2: Electric Boogaloo
categories: ruby,shell scripting
---

A few posts back I wrote a bash one-liner that would check syntax on all files in a directory. A friend of mine suggested I do the same thing with ruby. It's about 2:30AM and I can't sleep so maybe some boring code will do the trick.

The way to do this in a shell script would be:

~~~~{.sh}
#!/bin/bash
find $1 -name "*.rb" -type f -exec ruby -c {} > /dev/null
~~~~

In ruby, I'd do:

~~~~{.ruby}
#!/usr/bin/env ruby
Dir[File.join(ARGV.first, '**/*.rb')].each {|d| system("ruby -c #{d} > /dev/null")}"
~~~~

Turns out the shell way is actually a little prettier. That doesn't happen a lot.
