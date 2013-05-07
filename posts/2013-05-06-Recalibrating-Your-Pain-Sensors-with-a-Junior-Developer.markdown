---
title: Recalibrating Your Pain Sensors with a Junior Developer
categories: development, hiring
---

We recently underwent a round of hiring at work. Our first hire was a junior
developer. Previously, for the almost 2 years where we were understaffed and
had trouble hiring, we weren't looking for junior developers. We reasoned that
we could not afford the training cost because of how busy we were. Still, after
well over a year of running the ship on a skeleton crew, those fears eventually
wore away. Whatever the cost was, we had to pay it.

Bringing a new junior developer onboard is a huge investment for a team,
especially one strapped for time. The time investment goes beyond just getting
them used to the tools and the codebase (which in our case is fairly
substantial). A junior developer by definition will not have a lot of
experience in professional software development, maintaining production
systems, etc. Teaching a junior developer all of the best practices you've
learned over the years is a long journey. I for one have a hard time
remembering what it was like to be in their shoes, which is necessary for
setting expectations on how quickly they should be absorbing knowledge and how
much I need to explain.

I've never had to train a developer who was junior to me so this is still a
learning experience for me, but I have identified a few unexpected benefits
that have occurred along the way.

## Verbalizing Your Knowledge
Training a new developer involves a lot of talking. At work we have a dedicated
pairing station for the new developer. I sit with a pair of monitors, keyboard,
and mouse plugged into her machine and we pair program on problems. After
starting this training, I've really come to realize how much I rely on jargon
in day-to-day programming. I constantly catch myself saying things like
"memoize that", "delegate this", and "let's write a DSL for this".

This jargon is extremely valuable if you are working with somebody that
understands it. A big part of pair programming is staying on track. Even more
imporant, you should realize when you are moving ahead of your pair and be able
to quickly cast a line out with a carefully selected pattern and reel them back
onto the boat.

When working with a junior developer, I'm suddenly forced to prove that I
really know what these terms mean by reducing them to simpler terms that
someone can understand without ever having experienced the need to use them.
I even find myself seeking to justify our style guide in practical terms. Why
do we prefer `&&`/`||` `and`/`or`? Why should we prefer method access to instance
variables rather than referencing them directly? I never had to verbalize
method-to-proc in Ruby before. How could I explain it to someone who was on
shaky ground with procs to begin with?

## A Clean Slate
When I started at my company as an intern I was essentially put in a separate
room and left to my own devices. While the independence was nice sometimes, it
also lengthened how long it took me to get to proficiency. For a good solid
month, I fell in love with the `#returning` method introduced by ActiveSupport,
which later got reimagined as `#tap`. I used it all over the place. I convinced
myself that getting rid of local variables in exchange for block parameters was
a Good Thing. If just one senior developer would have challenged me to explain
why I believed this, my enthusiasm for `#tap` would have been appropriately
curtailed.


One of the nice things about training a junior developer is that you can see
cargo culting and misguided habits start to form and hopefully set the dev back
on the right track again. You may even identify and eliminate bad habits and
magical thinking you've aquired over the years while doing so.

## Recalibrating Your Pain Sensors
This one was really surprising to me. After you've been at a company for a
while, you begin to develop a high pain tolerance. Many developers, most
notably DHH, have discussed how pain can be used as a formative tool for your
software. A developer should be tuned to what things in the code pain them and
should use this to guide refactorings. James Edward Gray in his
[keynote](http://www.youtube.com/watch?v=ZLFeqmh7Dec) at Ruby Midwest talked
about how he has a higher tolerance for complexity and hacks than most and how
much of a curse this was as it dulled his ability to feel pain from code (to
paraphrase).

There are literally hundreds of areas in the code at work that just don't make
sense, require workarounds and slow development (or even worse testing). I
could rattle off dozens of them at the top of my head. It is so easy to use
this institutional knowledge as a crutch and never fix it. It doesn't hurt and it
lets me bitch about how wrong someone else before me got it. Why change it?

Then I found myself explaining this code to a new developer. I saw the excitement
drain from her eyes as we spent nearly an hour trying to tiptoe around a litany
of overreaching ActiveRecord callbacks just to write a fucking test for a
reporting tool. She put up with it because she figured that was just the way it
was. It was embarrassing.

There are no savings to be had from not addressing confusing, poorly-designed
code when you have a new developer. This code *lies* to the developers that
rely on it. New devs will spin their wheels, ask for help (hopefully), and
might even get themselves stuck. Ultimately, they'll become used to it.
Their pain sensors will dull and you'll be back to having a full team building
new shinies on top of a rotting foundation.  Some devs will burn out and leave.
I've definitely felt close to that point several times.

## Have a Breaking Point
Enough is enough. I'm making a conscious effort to heighten my sense of pain.
If I come across something that's been a thorn in my side for 3 years, I'm
going to test it, refactor it, and unceremoniously defenestate the remainder.
If that takes extra time then that's how long it is going to take. I will stand
my ground to anyone who would argue that we don't have the time for
refactoring. Software *is* maintenance. If I don't improve maintainability
then I'm not doing my job. If you honestly advocate taking on more technical
debt in projects as large and impoverished as some of ours, you can no longer
count yourself as having an interest in its long term success.
