---
title: Focusgrid jQuery Plugin Released
categories: jquery,javascript,projects
---

Just wanted to quickly announce that my company has released the focusgrid
jQuery plugin that I had been working on. You can download it on
[GitHub](https://github.com/crystalcommerce/focusgrid).

Usage
-----
Focusgrid is intended for a very specific use case: navigating a grid of input
cells in a table structure. It allows you to create a spreadsheet-like grid and
navigate it with your arrow keys quite easily using only the semantics of
tables. Options are documented in the readme. For the base case, all you need
to do to initialize it is:

~~~~{.javascript}
$('#myTable').focusgrid();
~~~~

We found this very useful for an inventory management system we were working on
gussying up. There are much nicer jQuery UI plugins for creating full-blown
spreadsheet like systems, but if you need to tack on easy key navigation in an
existing table, give it a try.

The project is released under the MIT license.
