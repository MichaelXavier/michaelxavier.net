---
title: SASSify a CSS File/Minify a JS File in 1 Line
categories: sass,one-liner,jsmin
---

It's been quite some time since I updated. I just started a new job and haven't had time to do much of anything. I recently had the need to quickly download CSS files and JS files and turn them into SASS/minified JS, respectively. One-liners to the rescue!

I've been using the -O- flag for wget ever since I've found out about it to simplify downloading and extracting files. Once you get your mind in the habit of using STDIN/piping in interesting ways, there's no stopping you.

To download a CSS file and convert it to SASS, you'll need HAML/SASS installed, which provide the sass-convert tool:

~~~~{.sh}
wget http://example.com/stylesheet.css -O- | sass-convert -F css -T sass -s > stylesheet.sass
~~~~

To download a JS file and convert it to minified JS, you'll need the jsmin gem installed. Also be careful doing this. If someone gets lazy and doesn't end a line with a semicolon in JS, the minified version will most likely fail to parse when you use it.

~~~~{.sh}
wget http://example.com/application.js -O- | ruby -rjsmin -e 'print JSMin.minify(STDIN)' > application.min.js
~~~~
