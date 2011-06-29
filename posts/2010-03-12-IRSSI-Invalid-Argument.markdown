---
title: IRSSI Invalid Argument
categories: irssi
---

If you're getting "invalid argument" in irssi, it is possibly because your hostname is misconfigured.

I'm not an expert on irssi but I use it often. I think the -h flag to specify
hostname (which I mistook to specity the *remote* hostname) somehow sets your
hostname to whatever you put after it to be persistent. This problem pretty much made it so I couldn't use irssi on this machine. The solution was very simple:

~~~~{.sh}
#When inside of irssi
/set -clear hostname
~~~~

And you should be good.
