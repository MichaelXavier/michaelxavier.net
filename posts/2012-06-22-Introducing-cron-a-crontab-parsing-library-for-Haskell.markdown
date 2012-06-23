---
title: Introducing cron, a crontab parsing library for Haskell
categories: haskell
---

I've been without a dev machine for the past week or so, but I'm finally
getting a chance to do a writeup on a project I just released.

## What Is It?
cron is a library on [hackage](http://hackage.haskell.org/package/cron) that I
created to parse crontab expressions with Haskell. I did this project because I
thought it would be relatively low hanging fruit and still teach me a lot about
writing slightly more complex parsers.

I got the idea from resque-scheduler, a Ruby library, which uses the cron
syntax in its schedule file to allow the user to configure how often a job will
be scheduled in a common, well-known format. A Haskell applicaiton that has use
for user-defined periodical tasks will do well to use a common format than
coming up with something more novel.


## Usage

System.Cron.Parser exports `cronSchedule` and `cronScheduleLoose` for parsing
just the schedule part of a crontab, i.e. `*/2 * * * *`. Use `cronSchedule` for
a parser that will fail on extraneous input after the cron expression. Code for
parsing just schedules may look something like:

~~~{.haskell}
import Data.Attoparsec.Text
import Data.Text
import System.Cron
import System.Cron.Parser

parseCronSchedule :: Text -> Either String CronSchedule
parseCronSchedule = parseOnly cronSchedule
~~~

You will also probably be interested in `scheduleMatches` from System.Cron.
This function is of type CronSchedule -> UTCTime -> Bool. You would use this
when developing a scheduler to see if its time to run a command or not.

## Writing a Naive Cron Runner

Let's write a simple little executable that behaves like cron. It will work
like this:

1. Read a cron config from ARGV.
2. Wake up every minute and see what it has to do.
3. For each job that matches the current time, fork off a process to execute
   it.


~~~{.haskell}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Concurrent
import Control.Monad
import Data.Attoparsec.Text
import Data.Monoid (mconcat)
import Data.Text (Text, pack, unpack, splitOn)
import Data.Time.Clock
import qualified Data.Text.IO as TIO
import System.Cron
import System.Cron.Parser
import System.Environment
import System.Posix.Process
import System.Posix.Types (ProcessID)

-- Do this in a much safer way in your real app!
main :: IO ()
main = do (infile:_)  <- getArgs
          contents    <- readFile infile
          let (Right tab) = parseCrontab . pack $ contents
          let schedule = digestSchedule tab
          forever $ work schedule >> sleep
  where sleep      = putStrLn "Sleeping" >> threadDelay delay
        delay      = 6000000
        parseCrontab = parseOnly crontab

work :: Schedule -> IO ()
work (entries, env) = mapM_ run . findDue =<< getCurrentTime
  where findDue time = filter (\(sched, _) -> scheduleMatches sched time) entries
        run (_, cmd) = runCommand env cmd
                   
type Env = [(String, String)]
type ScheduledEntry = (CronSchedule, Text)
type Schedule = ([ScheduledEntry], Env)

digestSchedule :: Crontab -> Schedule
digestSchedule (Crontab entries) = foldl extract ([], []) entries
  where extract (entries, env) (CommandEntry sched cmd) = ((sched, cmd):entries, env)
        extract (entries, env) (EnvVariable var val)    = (entries, (unpack var, unpack val):env)

runCommand :: Env -> Text -> IO ProcessID
runCommand env cmd = fork execute
  where fork io = log >> forkProcess io
        execute = executeFile cmdPath True args (Just env)
        (cmdPath:args) = map unpack $ splitOn " " cmd
        log = TIO.putStrLn . mconcat $ ["Running ", cmd, " with ", (pack . show) env]

~~~

Here's the sample cron file

~~~
# comment
# another
ENV1=wat
ENV2=huh
* * * * * echo "WEEE"
~~~

Let's compile and try it out!

~~~
$ ghc --make Main.hs -o hscron
[1 of 1] Compiling Main             ( Main.hs, Main.o )
Linking hscron ...
$ ./hscron samplecrontab
Running echo "WEEE" with [("ENV2","huh"),("ENV1","wat")]
Sleeping
"WEEE"
~~~

Not quite as slick as the real thing but not too shabby for either. If you can
think of any additional functionality that make sense in the library, please
drop me a line on the [issue
tracker](https://github.com/michaelxavier/cron/issues) and I'll have a look.
