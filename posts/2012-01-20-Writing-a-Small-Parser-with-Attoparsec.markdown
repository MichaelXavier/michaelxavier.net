---
title: Writing a Small Parser with Attoparsec
categories: haskell, attoparsec
---

A word of caution: This post features code from my first and so far only parser
I've ever written. I was able to achieve what I wanted by reading attoparsec's
documentation, which is by no means written in the manner of a tutorial. I let
the types and my understanding of the various typeclasses it uses guide me. I
make no claims that this is The Best Way® to do things. The parser I made is
performant enough for my needs but I have done no formal benchmarks nor have I
put time into optimizing it further.

## Attoparsec

[Attoparsec](http://hackage.haskell.org/package/attoparsec) is a Haskell
library for creating parser combinators. It is inspired by the older Parsec
library and is designed with performance and efficiency in mind. Brian
O'Sullivan is the creator. Brian writes some very high-quality libraries, all
of which are a joy to use.

I decided to write this because I found very few tutorial-like resources for
writing parsers with Attoparsec. The documentation is good once you know a bit
of what you're doing, but there wasn't anything to get started. I dug into the
source code of another one of Brian O'Sullivan's projects,
[Aeson](https://github.com/bos/aeson)

## The Problem

I needed a parser for a project I'm working on,
[HollaBack](http://github.com/MichaelXavier/HollaBack). This service receives
emails with a specified date as the mailbox, parses them and bounces the email
back to you at the desired time. I got the idea from
[FollowUp.cc](http://www.followup.cc). Since their format seemed as good as
any, I decided to make the parser for their date format.

## The Format

The date format can either be a *relative* time or a *specific* date/time. A
relative date/time looks like (quantity)(time keywword). Time keywords are mi,
h, d, w, mo, and y. Some examples are:

* 2d
* 3mo
* 45mi

Specific date/times look like:

* jan5
* march14-2pm
* friday-5am
* sunday
* 8am

## The Types

These should be pretty self explanatory.

~~~{.haskell}
data DayOfWeek = Monday    |
                 Tuesday   |
                 Wednesday |
                 Thursday  |
                 Friday    |
                 Saturday  |
                 Sunday deriving (Show, Eq)

data Date = Date Month Int deriving (Show, Eq)

data DateTimeSpec = RelativeDateTime TimeUnit               |
                    SpecificDateTime Date TimeOfDay         |
                    SpecificWeekdayTime DayOfWeek TimeOfDay |
                    SpecificWeekday DayOfWeek               |
                    SpecificTime TimeOfDay deriving (Show, Eq)

data TimeUnit = TimeUnit Integer TimeKeyword deriving (Show, Eq)

data TimeKeyword = Minutes |
                   Hours   |
                   Days    |
                   Weeks   |
                   Months  |
                   Years deriving (Show, Eq)
~~~

## The Imports

~~~{.haskell}
import Control.Applicative ((<*>),
                            (*>),
                            (<$>),
                            (<|>),
                            pure)
import qualified Data.Attoparsec.Text as A
import qualified Data.Attoparsec.Combinator as AC
import Data.Attoparsec.Text (Parser)
import Data.Text (Text)
~~~

## Writing Parsers
Parsers can be written almost entirely in terms of functions from
Control.Applicative. Try LYAH for a [refresher on
Applicative](http://learnyouahaskell.com/functors-applicative-functors-and-monoids#applicative-functors).

The first step is to define signatures for combinators for all the major data types:

~~~{.haskell}
timeUnit :: Parser TimeUnit
timeKeyword :: Parser TimeKeyword
day :: Parser DayOfWeek
time :: Parser TimeOfDay
date :: Parser Date
~~~

### Parsing Discrete Values
Parsing discrete values is the simplest. Lets start with `day`

~~~{.haskell}
  day :: Parser DayOfWeek
  day = monday    <|>
        tuesday   <|>
        -- ...
    where monday = A.stringCI "monday" *> pure Monday
~~~

We want to match the string "monday" so we use stringCI, which does a full
string, case insenstive match. stringCI has a type `Text -> Parser Text`. We
use `*>` which discards the result of the first action (Parser Text), and
returns the second. pure lifts the value Monday into the functor.

Luckily for us, Parser has an instance for Alternaative which is a "monoid on
applicative functors". `<|>` is an associative binary operation. in the case
of a paaarser, if the first parse fails, the next parser is used. So when we
chain together these parsers with `<|>`, it will try them sequentially until
one is successful.

Adding multiple aliases for each day is easy:

~~~{.haskell}
    stringChoices :: [Text] -> Parser Text
    stringChoices = AC.choice . map A.stringCI

    -- ...
    where monday = stringChoices ["monday", "mon"] *> pure Monday
~~~

### Combining Parsers

For compound types like TimeUnit, the best way is to use Applicative's
sequential application function, `<*>`. We'll write a parser for each component
of TimeUnit, one for the quantity and the other for the TimeKeyword.


~~~{.haskell}
    timeKeyword :: Parser TimeKeyword
    timeKeyword = minutes <|>
                  hours   <|>
                  -- ..
      where minutes = A.stringCI "mi" *> pure Minutes
            hours   = A.stringCI "h"  *> pure Hours
            -- ..
~~~

We can think of the TimeUnit constructor as a function that consumes arguments
(I know this isn't exactly how it works in Haskell, but I'll describe it thusly
for brevity). Thus, the type is:
`TimeUnit :: Integer -> TimeKeyword -> TimeUnit`. Our parser will then look
like this:


~~~{.haskell}
    timeUnit :: Parser TimeUnit
    timeUnit = TimeUnit <$> integer
                        <*> timeKeyword
      where integer = toInteger <$> A.decimal
~~~

Decimal is capable of parsing floating point numbers as well, but because we
use fromIntegral, the parser will fail if it is given a floating point
quantity, which is exactly what we want.

## That Was Much Easier Than Expected

I'm really quite impressed by the ease of use for parsing simple grammars with
Attoparsec. The Applicative/Alternative instances make the parsers read like
BNF  grammar notation and make parsing complex types a simple matter of
creating smaller parsers and then composing them.
