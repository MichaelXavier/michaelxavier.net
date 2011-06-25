---
title: RR's any_instance
categories: RR,Ruby
---

For a while I could blame the lack of updates here on my internship. It has long since been due to pure laziness. I've been using [RR]{http://github.com/btakita/rr} for stubbing/mocking in any projects that will allow it. Until recently, the documentation was lacking for any_instance_of. Why write this post then? Shut up. That's why.

You should use any_instance_of when you cannot control the instantiation of a
class but must stub out a method on that instance. Say for example that you
have a method you are testing which must create and modify several instances of
*User*. You don't want to couple your tests to a lot of internal signals used
on those User instances or even their instantiation. However, you know that the
tests will make a call to some external resource you don't want to hit. RR's
got your back.

    #!sh_ruby
    describe Client
      subject {Factory.build(:generate_refunds)}
      describe "#generate_refunds" do
        it "returns only successfully refunded purchases" do
          any_instance_of(Refund, :send_to_paypal => false)
          subject.generate_refunds.should be_empty
        end
      end
    end

I know I've come across a better use case but I can't think of it right now.