---
title: "Ruby Tip: Return Explicitly From Block"
categories: ruby,tip
---

Sometimes when you use blocks you need to do something with the return value of a block. Using the last evaluated statement in the block can be hard to follow for others.

Consider the following trivial code. The actual scenario where I figured this
out was much more complex and meaningful, of course.

~~~~{.ruby}
def foo
  10.times do |n| # We don't want to 'next' to this loop. Simply to prove a point.
    puts "in loop #{n}"
    return bar do |from_bar| # We want to return from this method in the first loop.
      puts "got \"#{from_bar}\" from bar."
      next "foo done" # This code will be the block's return value but will *not* apply to the outer loop.
    end
  end
end

def bar
  puts "in bar"
  ret = yield "hello from bar"
  puts "back to bar land. got \"#{ret}\" from caller"
  return "bar, signing out"
end

puts "foo returns \"#{foo}\""
~~~~

Here's what you get when you run it:

~~~~{.sh}
in loop 0
in bar
got "hello from bar" from bar.
back to bar land. got "foo done" from caller
foo returns "bar, signing out"
~~~~

This may be preferable to just putting your return value somewhere in the block
whereit will evaluate last. If you're building a complex block, first consider
refactoring. If you can't, you may want to use this 'next' trick. When you or
someone works on the code later, they may not consider that the block must
return a value, add some code at the end and completely throw off the intended
return value.
