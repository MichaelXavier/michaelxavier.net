---
title: Inheritance with state_machine
categories: ruby,state_machine
---

I'm working on a project now that requires some state machine features in some of my classes. I tried acts_as_state_machine but I found the documentation to be awful and the library itself to be less feature-rich that state_machine. state_machine claims to support inheritance but I found no examples of this anywhere.

Below is a simple example of inheritance that seems to work for me with state_machine. There could be a better way to do it but I wasn't able to find it. In this example, the initial state is inherited from the base class.

~~~~{.ruby}
class Procedure
  state_machine :initial => :not_started do
    #More Procedure-specific events and whatnot
  end
end

class BusinessProcedure < Procedure
  state_machine do
    after_transition :on => :start_work do |obj|
      obj.inflate_billable_hours
    end
  end
end
~~~~

Stay [DRY](http://en.wikipedia.org/wiki/Don't_repeat_yourself) folks! By the way, I am aware of the weird indentation going on in my code samples (and the lack of code highlighting). The lack of code highlighting is because I haven't implemented it yet. The weird indentation is because Markdown is not performing as it should. I will fix it eventually.
