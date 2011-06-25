---
title: IShack Util Released
categories: ruby,github,ishack
---

I find myself posting links to images in IM conversations all the time. I wrote a little utility to make it easier.

Yeah I realize this is a pretty simple open source contribution but it's all I had time for for now. Check it out on my [GitHub]{http://github.com/MichaelXavier/IShack}.

Usage
-----

Run ishack --help for usage, but in general:

    #!sh_sh
    ishack myfile1.jpg myfile2.jpg
    ishack -t http://example.com/1.jpg http://example.com/2.jpg # transloading from other urls

TODO
-----
1. It would be cool for the progress bar to keep track of data uploaded. Right now it just increments after each file, which is pretty much useless for small numbers of images.
2. Output is aligned kind of weird. This should be an easy fix.
3. STDIN file input. You could transload and resize like: wget http://example.com/1.jpg -O- | convert - -resize 50% - | ishack


Oh, and I just found out tonight that my other little project MechaZilla must have a really stupid implementation bug in it because it took up > 1GB of ram in a large download. Whoops.