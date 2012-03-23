---
title: Starting Resque Workers Sequentially
categories: ruby, rails, resque
---

## Why Start Sequentially?
At work we run a main [resque](https://github.com/defunkt/resque) worker server
that has about 15-20 workers running at any given time. We also distribute
about 5 or 6 workers to our app servers to get more work done and for security
against our main worker server going down. Sometimes, our clients cause
**hundreds of thousands** of jobs to get enqueued in a very short period of time.
To combat this we will spin up a clone or two of the worker server with 30 or
so workers each to handle the extra burden.

You typically start workers like this:

`QUEUE=* rake resque:work`

We use [god](http://godrb.com) to start up all of our workers this way. This
worked pretty well for a while.

*Here's the rub*: starting 20 workers on a single machine is brutal. Really
brutal. All 20 workers are trying to start a heavy rails app environment and
are getting into resource contention. This results in load on a 4 core system
going up to something like 10. After a deploy, our workers would take upwards
of **10 minutes** to start.

![](/assets/errata/resque_bad_time.jpg "You're gonna have a bad time")

## Solution: Add File Locks in the Rakefile

The solution is to start workers sequentially on the same machine by hooking
into the worker tasks in rake. This is what worked for us:

~~~{.ruby}
# lib/tasks/resque.rake
require 'fileutils'

# Copied from http://thomasmango.com/2010/05/27/resque-in-production/
namespace :resque do
  LOCKFILE = File.join(File.dirname(__FILE__), '..', '..', 'tmp', 'worker_start.lock')

  desc "wait for lock file to clear before starting"
  task :wait_for_lock do
    begin
      File.open(LOCKFILE, File::CREAT | File::EXCL | File::WRONLY) do |f|
        f.write(Process.pid)
      end
    rescue Errno::EEXIST => e
      sleep(1)
      retry
    end
  end

  task :clear_lock do
    FileUtils.rm(LOCKFILE, :force => true)
  end

  task :preload => [:wait_for_lock, :environment] do
    # Add clear lock to run after loading rails
    Rake::Task['resque:clear_lock'].invoke
  end
end
~~~

There is a much more elegant solution in Ruby's
[File#flock](http://ruby-doc.org/core-1.9.3/File.html#method-i-flock) but it
didn't seem to work in this case. Every contrived script I could come up with
would have an exclusive flock (file lock) would work, but in a task like this,
the lock would not be exclusive and all the workers would start at once. This
solution is ugly but does the trick. Without so much resource contention, our
workers are each starting in < 30 seconds and can begin work immediately while
the other workers are waiting.
