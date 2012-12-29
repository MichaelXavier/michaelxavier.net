---
title: Introducing Buster
categories: haskell, projects
---

![Hello brother](/assets/errata/buster.jpg "Hello brother")

## Introducing Buster

Buster is a utility for hitting a list of URLs periodically.

At work, we hit certain sites with a special URL parameter that causes the full
page cache to be rebuilt every 5 minutes. Previously, this was done in a cron
task. That's fine but I felt this could be a good learning experience and would
end up being more configurable. By the way, if you do want to use curl/cron to
accomplish this task, be sure to redirect output to /dev/null or you'll end up
with a multi gigabyte spool file from cron!


## Getting Buster

Either clone from my [GitHub](http://github.com/MichaelXavier/Buster) or
download it from [HackageDB](http://hackage.haskell.org/package/Buster). It
will install an executable called `buster`

## Usage

Run `buster config.yml`

See the README and examples directory for what a config should look like.
Buster supports optional automatic config file reloading or reloading via a HUP
signal.

## Reflections

At this stage in my Haskell career, if I embark on a new projects, its because
I want to learn some new things along the way. Buster itself isn't that
compelling of a utility, but the learning experience was.

### YAML

Since I work in a Ruby shop, I thought it would be a good idea to have the
configuration be in YAML. That way, coworkers could pretend they are using a
normal Ruby executable without noticing Haskell's slow infiltration into day to
day operations.

The yaml package is interesting because it uses the mighty `aeson` parsing
combinators. That was kind of weird but it made writing the config parser
simple:

```{.haskell}
instance FromJSON Config where
  parseJSON (Object v) = Config <$> v .:? "verbose" .!= False
                                <*> v .:? "monitor" .!= False
                                <*> v .:  "urls"
                                <*> v .:? "log_file"
  parseJSON _          = fail "Expecting Object"

instance FromJSON UrlConfig where
  parseJSON (Object v) = UrlConfig <$> v .: "url"
                                   <*> v .: "interval"
                                   <*> v .:? "method" .!= "GET"
  parseJSON _          = fail "Expecting Object"
```

### Config Reload with hinotify

`hinotify` is a pretty thin abstraction over linux' inotify feature for
monitoring files. I'd used it before in Ruby but I was pleased with being able
to monitor the config without resorting to polling.

### Exceptions Runing My Life

I started writing a section here and then realized there was too much in my
head to confine it to a section in an introductory post. Keep an eye out for a
separate post on exceptions in Haskell and why I hate trifling with them. Long
story short: the `errors` library and the EitherT monad transformer helped a
great deal. I tried my best to intercept exception-throwing interfaces (though
who knows since the goddamn type doesn't communicate exceptional behavior) at
low levels and normalize them to an Either interface. I have decided that if
there are going to be errors in my code, the compiler should not allow my
program to compile without confronting them in some manner.

### Testing with Temporary Files

Keep an eye out for a small post about this. I wanted to be able to test the
parsing of actual config files without having to worry about explicit cleanup.
The `temporary` package is really nice for tasks like this.
