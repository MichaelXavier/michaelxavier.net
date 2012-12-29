UNFINISHED

I'm still fairly ignorant about exceptions in Haskell, but after having to
really confront them first time in this project, I hate them. Hate is a strong
word, and I mean it. Considering how much safety Haskell's type safety affords
the programmer, exceptions seem like a gaping breach in that safety. It's like
being an astronaut abord a space shuttle built to rigorous, strict safety
specs, and then NASA telling you that sometimes the fuel line explodes if you
don't press the green button under the dashboard every once in a while.

A function which throws exceptions is almost as bad as a partial function to
me. The types of exceptions that can be thrown are *not* encoded in the type.
You have to rely on every library maintainer to document which types of
exceptions can be thrown (spoiler: they don't).

I learned a great concept from Avdi Grimm called
[Confident Code](http://www.youtube.com/watch?v=T8J0j2xJFgQ). You want your
code to be confident, not paranoid. Haskell's exception system forces you to be
paranoid because you either have to wrap monadic actions in exception handlers
*or* live in fear that any one of the libraries you use may throw an error,
which will bubble up and crash your beautiful, type-safe program. This isn't
necessarily unique to Haskell but if I wanted to live in fear of exceptions I'd
write Javascript.

Look at all the
[bullshit](https://github.com/MichaelXavier/Buster/blob/master/src/Buster/Util.hs#L35)
you have to put up with to catch all exceptions except the ones that you'd
better not catch (such as program exits). All of this to make sure that a rogue
exception doesn't crash my thread and render Buster useless.

### Coping With Exceptions

Pessimistically, exceptions are a way for a programmer to delay the
confrontation of errors. The programmer *can* add a handler 
