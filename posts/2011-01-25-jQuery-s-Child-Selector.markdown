---
title: jQuery's Child Selector
categories: jquery,javascript
---

Good Things to Come (I Swear)
-----------------------------

It has been quite some time since I posted on this blog. I've still been coding
a lot but I got lazy and couldn't come up with anything i wanted to write
about. I promise, however, that I'll soon be doing a writeup on some
experiments I did with node.js and backbone.js so keep an eye out for that.

On jQuery Selectors
-------------------

Now on to the topic at hand. If you are at all familiar with CSS2 and CSS3
selectors you'll know just how powerful and expressive they can be despite
their rather terse syntax. If you take a look at [jQuery
selectors](http://api.jquery.com/category/selectors) you can see the influence.
In the places where jQuery selectors innovate, they still try to make the code
analogous things like CSS pseudo-classes. The
[:first](http://api.jquery.com/first-selector) selector is a perfect example.
At work we have been developing a jQuery plugin which facilitates navigating a
table full of inputs in a pretty clean way using only the semantics of HTML
tables. I'm going to try to get that open-sourced so keep an eye out for that
plugin in the near future under the name "focusgrid". While I was working on
this plugin, I found the need to operate on direct descendant. Thats where
jQuery's parent child operator comes in (parent > child). There are some good
examples in the [documentation](http://api.jquery.com/child-selector). If you
are familiar with the CSS implementation, it works just the same.

Child Selector Example
----------------------

Say you had the following HTML and you wanted to find all images which were
direct descendents of elements with the class *image_container*

    #!sh_html
    <img src="decoy.jpg" />
    <div class="image_container">
      <img src="bingo.jpg" />
      <span class="image_container">
        <img src="alsobingo.jpg" />
      </div>
    </div>

Your jQuery would look like;

    #!sh_javascript
    $('.image_container > img')

This would return you the images for bingo.jpg and alsobingo.jpg, omitting decoy.jpg.

Using Child Selector With Implicit Parent
-----------------------------------------

In my case, I was in a nested loop and was to be selecting from an
element rather than from the global scope. In my case I was scoped to a table
and looking for TR tags *at the first level*, potentially nested underneath a
TBODY, THEAD or TFOOT tag, but *not* in a nested table. As it turns out
jQuery's selector will use the currently scoped element(s) as an implicit
parent in the parent > child selector syntax, allowing you to leave the parent
out entirely.

    #!sh_html
    <table>
      <thead>
        <tr class="findme">
        </tr>
      </thead>
      <tbody>
        <tr class="findme">
          <td>
            <table>
              <tr class="dontfindme"></tr>
            </table>
          </td>
        </tr>
      </tbody>
    </table>

Your jQuery would look like:

    #!sh_javascript
    var $table = $('table:first'); // Say we had to be scoped to the table
    $table.find('> tr, > thead > tr, > tbody > tr, > tfoot > tr'); // This will get both findme's and skip the dontfindme

The left hand operand of the parent/child operator is implicit in this case so
you won't have to do another (expensive) document scope find with an even more
complicated selector. Pretty neat.