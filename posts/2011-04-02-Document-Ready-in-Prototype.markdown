---
title: Document Ready in Prototype
categories: prototype,javascript
---

I came across an issue the other day on a production app at work that relied up
on prototype.js. I'm not a big fan of prototype but it was the default in rails
2 and switching to jQuery is currently more trouble than it's worth.

Nevertheless, someone had been using dom-manipulating code that didn't wait for
the page to load. Tsk tsk. I quickly looked up what prototype's document ready
equivalent was and found this:

~~~~{.javascript}
//In jQuery you'd do something like this:
$(function() { console.log("loaded!") });

//In prototype:
document.observe('dom:loaded', function() { console.log("loaded!") });
~~~~

One important detail is that both of these calls wait for the *dom* to load,
not the window. This means that your javascript will execute when the dom is in
its final state, but before any images or other resources are loaded. Most of
the time you do not want to wait for all these additional resources to load
before executing your code.

The problem lies in the semantics bteween the two libraries. If the page is
already loaded and you execute the jQuery snippet again, it will execute. It
detects that the dom is already loaded and does the reasonable thing: executes
the contained callback right away.

Prototype's snippet is purely event based. If that code is put on the page for
execution after the dom is loaded, the callback will *never* get called. This
is a bit of an edge case. In Rails, if you have a partial containing javascript
like this that can be loaded on a full page load and as part of an AJAX call,
you could get bitten by this gotcha.

The solution I came up with is admittedly inelegant but it gets the job done.

~~~~{.javascript}
(function() {
  var cb = function() { console.log('loaded')};
  if (document.loaded) {
    cb();
  } else {
    document.observe('dom:loaded', cb);
  }
})();
~~~~

jQuery, in my opinion provides a more sensible default. I have not torn through
the prototype docs to see if there's a better way to do this but the above code
seems to get the job done reliably.
