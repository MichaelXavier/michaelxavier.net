---
title: Shell Script for Checking Ruby Syntax
categories: ruby,shell scripting
---

I wrote a quick one-liner today for checking ruby syntax recursively. 

It's simple but it would be a good idea to run this _before_ committing changes to your repo. I can't tell you how many times I've gone through the long, annoying process of updating our windows rails server (I know...) with code that has a stupid syntax error in it.

If I find it useful, it might even be worth adding to pre-commit hooks on a ruby git repo.

~~~~{.sh}
#!/bin/bash
find $1 -name "*.rb" -type f -exec ruby -c {} > /dev/null
~~~~

This will redirect all the "Syntax OK" messages to /dev/null and will only print files that have errors. Told you it was simple.
