---
title: Snippet: Get all but the last n elements from Array
categories: ruby,code snippet
---

Here's how to get all of the elements of an array but the last n without knowing the length of the array.

I had the need the other day to find all but the last 2 elements of the array. You can use ranges pretty easily to do this but in my circumstance, I was in a chain of method calls and didn't want to create a local variable just to do this. The answer is simple:

~~~~{.ruby}
#Get the last 2
n = 2
[1, 2, 3, 4, 5, 6][0..-n - 1] #You can use the .. range syntax if you offset by 1.
[1, 2, 3, 4, 5, 6][0...-n]    #Or you can use ... syntax to forgo the offset.
~~~~

It's easy to forget all these little convenience methods that Ruby affords. While I found out how to do this, I also remembered that __last__ takes an optional argument so you can concisely take the last n elements like so: 
    
~~~~{.ruby}
[1,2,3,4,5,6].last(2)
~~~~
