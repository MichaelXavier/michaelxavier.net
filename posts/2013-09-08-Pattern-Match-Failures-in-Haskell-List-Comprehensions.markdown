---
title: Pattern Match Failures in Haskell List Comprehensions
categories: development, haskell
---

I stumbled across something quite nice the other day working with list
comprehensions in Haskell. I don't typically pull out the list comprehension
hammer very often. That's not because I dislike them, but more that I seem to
forget they exist. I found a pretty killer feature of them: If your pattern
match fails in a list comprehension, rather than causing some sort of
horrendous error, that element simply won't make it past the guard in your list
comprehension.

For example: in a project I'm working on called
[vigilance](http://github.com/michaelxavier/vigilance) I need to select watches
and then from those, pull email addresses associated from those watches to
notify them.

~~~{.haskell}
data NotificationPreference = EmailNotification EmailAddress |
                              HTTPNotification URL

extractEmails :: [Watch] -> [(Watch, EmailAddress)]
extractEmails ws = [ (w, addr) | w <- ws, (EmailNotification addr) <- notifications w]
~~~

There's a couple cool things going on here. First, we're extracting an element
from the watch list and then from that extracting notifications from each
email. I didn't realize you could do that. The really cool part is that the
pattern match failure didn't cause an error. Instead, it rejected any elements
that didn't match the pattern. I like this because it concisely expresses the
algorithm while still being perfectly readable.
