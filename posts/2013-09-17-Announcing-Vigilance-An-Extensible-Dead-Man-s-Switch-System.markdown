---
title: "Announcing Vigilance: An Extensible Dead Man's Switch System"
categories: haskell, projects
---

## Introducing Vigilance

Vigilance is a
[dead man's switch](https://en.wikipedia.org/wiki/Dead_man%27s_switch) system
written in Haskell.  The idea is that you register periodical tasks that you've
configured elsewhere such as:

* Backups
* Periodical billing
* Scripts that you run periodically

You tell vigilance an upper bound of how often the task should run via a config
file. You tell vigilance how to contact you if this doesn't happen (currently
HTTP POST and emails). The task can check in to vigilance with either the
`vigilance` executable or via a REST API. If it doesn't, check in, you will be
notified and vigilance will hold off on other notifications until your task is
back online, checking in again.

I created vigilance after a couple of incidents in which I discovered that
backups on my production systems or my development box had not been running. I
wanted a way to set a failsafe so that I could at least be alerted when
something like this happens.

Check it out on [github](http://github.com/MichaelXavier/vigilance) for the
full documentation. Download vigilance via `cabal install vigilance` or via
[hackage](http://hackage.haskell.org/package/vigilance).

## Example

Say I have daily backups that run on my server. I want to be notified if the
server goes more than 36 hours without *completing* a backup.

First, I install vigilance from cabal:

~~~
cabal update && cabal install vigilance
~~~

This provides 2 executables, `vigilance` and `vigilance-server`. You can
configure the build ot not include the server on client boxes for a faster
install with fewer dependencies. `vigilance-server` will handle the state of
your watches, notifications, etc. `vigilance` is what you will use to manage
your watches and do check-ins.

Let's write a config file to `~/.vigilance/server.conf` (the default location):

~~~{.conf}
# ~/.vigilance/server.conf
vigilance {
  port = 9999
  watches {
    backups {
      interval = [36, "hours"]
      notifications = [
        ["email", "me@example.com"],
        ["email", "joe@example.com"],
        ["http", "http://example.com/in-case-of-emergency"]
      ]
    }
  }
}
~~~

Vigilance tries its best to have reasonable defaults for its configuration. All
state data and logs will be stored in a `.vigilance` directory in your user's
home directory. Fire up your server by running:

~~~
vigilance-server
~~~

Since we're using a non-standard port for our client, let's write a config at
`~/.vigilance/client.conf`

~~~{.conf}
vigilance {
  host = "localhost"
  port = 9999
}
~~~

Your crontab would look something like this:

~~~
@daily run_backups.sh && vigilance checkin backups.
~~~

If run_backups.sh blows up, 2 emails will be fired off and an HTTP post of the
failure will be posted to your callback URL.

Adding or removing watches is as simple as editing the `server.conf` file and
sending a HUP signal to the `vigilance-server` process:

~~~
kill -HUP pid_of_vigilance_server
~~~

If you have any ideas on more notifiers you'd like to see added, please file an
issue on the 
[github issue tracker](http://github.com/MichaelXavier/vigilance/issues).
