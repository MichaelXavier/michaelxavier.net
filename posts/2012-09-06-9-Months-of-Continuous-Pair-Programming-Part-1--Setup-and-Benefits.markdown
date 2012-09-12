---
title: 9 Months of Continuous Pair Programming Part 1: Setup/Reasons
categories: pair_programming software_development
---

This is the first in a series of posts about my reflections of continuous pair
programming. Continuous pair programming is spending most of your time (say
80%) pair programming.

## Our Pairing Setup
No pair programming post could be complete without a rundown of technology.

We have tried over-the-shoulder pair programming at one machine and found that
it squanders the benefits of pairing. The dev who isn't driving must risk
interrupting the flow of the other dev if he wants to try something. If he
finds something problematic, he makes this known by pointing over the other
dev's shoulder. The whole practice feels more like a critique than
collaborative coding.

Our pairing setup allows either person to type at any time and allows one
developer to break off onto their own machine to try something out or do
research without interrupting.

Here's what we've got:

* 2x Ikea Jerker desks. These desks are discontinued but can be easily
purchased on craigslist for < $80. They are sturdy, and easy for 2 people to
build. We use them in a standing configuration but can be used as a decent
sitting desk if that's not your thing. If you intend to stand, get some good
shoes, insoles if you have high arches and a good quality standing mat or this
will kill your feet.

![Desk side view](/assets/pair_programming/side.jpg "Desk side view")

* 1x Dual-monitor DVI KVM switch such as
[this one](http://www.newegg.com/Product/Product.aspx?Item=N82E16817402022).
This allows one pair to have the pairing host machine and the other to be able
to switch back to their own machine. This model is nice because it supports key
combinations for switching an and audable beep when switching which gives a
cue to the other person that you are switching in/out of their computer so they
know what you're doing.

![Dual monitor KVM Switch](/assets/pair_programming/kvm.jpg "Dual monitor KVM Switch")

* An editing configuration that can be agreed upon. God help you if you get
into an editor religious war.

## Reasoning

Continuous pair programming is a term I've probably made up. At my job, I am
one of only two developers. This is not because we are a scrappy, lean startup.
This is not because having only 2 developers is a best practice. If you have
hard stuff to do, 2 developers is usually not enough. We have 2 developers
because, as it turns out,  it is difficult to hire Rails developers that know
what they are doing and have enough experience to hit the ground running.
[email me](mailto:michael@michaelxavier.net) if you are an experienced Rails
developer looking for a job.

About a year and a half ago, the primary developers on our platform left. This
left us, the remaining 2 developers, with a 40K+ line codebase that spanned 4
or 5 years and a mountain of technical debt, bugs, and features that are
needed. Continuously pair programming seemed like the best choice for the
following reasons.

### Risk Mitigation
Maintaining a large, fairly crusty codebase with lots of
active customers is risky business. At this point I was a moderately skilled
developer but my refactoring and testing skills weren't refined to the point
where I could handle large amounts of poor quality code. Setting out on new
features was difficult because I didn't undertand the entire system and didn't
have much experiencing developing features from scratch.

Code reviews did not work for us. With few developers, finding time to do
in-depth code review is hard. Doing cursory code review proved nearly
worthless, because the reviewer is looking at a final product with less mental
context than the author had, so they're lucky if they can catch any insidious
bugs.

### Insulation From the Business Division
Every developer in a small startup knows the constant struggle between
engineering wanting to focus and the business side wanting to get every pet
project done ASAP. When we had more devs, the more junior devs were often
pulled aside and siloed on work by the business team.

*Fuck. That.* If I could change only one decisions I've made at my current job,
it would be to refuse that siloing. Standing up to that would have been worth
staking my job on. It is a *terrible* idea, because it makes it easy
for senior developers to disengage from the junior developer and forces the
fledgling junior developer to struggle without having seen the "right way" to
approach a decent sized problem from start to finish. Lots of bad, poorly
tested code gets written that way that must be fixed later.

I should also mention that I felt a great deal of isolation at that time. Sure,
I had a problem all my own, but I was also in another room from the senior devs
and didn't feel like I could interrupt their flow with my task that they'd
never heard of. At that time I was miserable and started to become
disillusioned about software development. Do not put a developer through this
if you intend to keep them around.

By moving to a pair system, we focus and work as a group. The flow of
alternative perspectives is constant and the business office no longer got the
option of dividing the team and micromanaging development resources
independently of eachother.

### Using Singular Focus as an Advantage
As a 2 man operation, we could only ever hope to focus on 2 things at once. 2 is
close enough to 1 that we decided to turn this deficit into a benefit. We
decided to focus on exactly 1 thing at a time as a pair. This means that we
optimally have 2 people paying very close attention to the code being written.
For most important code written during pairing time, it means that no code gets
committed and deployed that another person did not have a direct hand in
producing.

Think about the power in that. If something goes wrong, both engineers can
think back to code that *they wrote* that may have caused the breakage.
Debugging ends up being much quicker.

Singular focus also made planning which tickets from the ticketing system to do
much easier. We would focus on one feature and see it through to completion,
with continuous oversight to make sure no step in the implementation was
missed.

## On the Next Episode
In my next blog post in this series, I'll go into what I've learned about what
works and what doesn't. I'll reflect on how to be a decent pair programming
partner, something at which I could probably stand to improve.
