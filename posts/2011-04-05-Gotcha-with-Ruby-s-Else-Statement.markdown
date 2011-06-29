---
title: Gotcha with Ruby's Else Statement
categories: ruby,gotcha
---

I came across a strange gotcha with the *else* statement in ruby. 

As it turns out, you can place an expression directly after an *else* statement:

~~~~{.ruby}
if false
  puts "you'll never see this"
else false
  puts "you'll always see this"
end
~~~~

At first I chalked it down as a flaw in the grammar, but it makes sense when
you examine it. The ruby grammar expects an expression to follow else. It does
not necessarily have to occur on the next line. A line break separates the
false and the puts statement following the else. In this case, if the else
statement is to be entered at all, the expression on the same line will
*always* be evaluated, as will the code segment that follows within the else
clause.

This may seem silly but I caught a fairly serious bug that came as the result
of someone using an else instead of an elsif. Sometimes it's fun to dissect an
interesting edge case in the language.
