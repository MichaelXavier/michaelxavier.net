---
title: What you don't know about cattr_accessor can kill you
categories: ruby,rails,tips
---

I found myself bitten by my misunderstanding of how class variables worked and found the actual functionality to be somewhat unexpected.

_cattr_accessor_ is a convenience method added via ActiveSupport as I recall. It works much in the same way of _attr_accessor_ and comes in the form of _writer_ and _reader_ as well. Unfortunately, this convenience led me to assume that _cattr_accessor_ is to inheritance what _attr_accessor_ is to individual instances of a class. In other words, I figured that changing a class variable in a subclass would change it only for that subclass. Such is not the case.

Consider the following example:
     
~~~~{.ruby}
class Fruit
  @@delicious = true
  cattr_reader :delicious
end

puts "Fruit is delicious: #{Fruit.delicious}"          #Prints true

#HoneyDew is gross
class HoneyDew < Fruit
  @@delicious = false
end

#One would think this would inherit the class variable. One would be wrong.
class Banana < Fruit
end

puts "Fruit is delicious: #{Fruit.delicious}"         #Prints false 
puts "HoneyDews are delicious: #{HoneyDew.delicious}" #Prints false 
puts "Bananas are delicious: #{Banana.delicious}"     #Prints false  
~~~~

You will notice that the class variable is shared and that it is modified in the order that the code is evaluated. I made a bad decision to use code like this in an application I was writing and it caused some very unexpected behaviour.

I would suggest just defining class methods and leave out the class variables entirely. On writers, this allows you some control and error checking for potentially undesirable input and is overall a more forward, if not slightly more verbose approach.
