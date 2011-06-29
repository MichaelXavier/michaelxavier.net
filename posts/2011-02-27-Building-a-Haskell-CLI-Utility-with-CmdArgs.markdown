---
title: Building a Haskell CLI Utility with CmdArgs
categories: haskell,cmdargs
---

## CheckPt

Recently I completed the first version of
[CheckPt-Haskell](http://github.com/MichaelXavier/Checkpt-Haskell), my first
Haskell project of any note. It is essentially a glorified checklist to help me
keep track of tv shows, books, etc that I intend to watch. 

## The Case for Subcommands

When I initially planned on implimenting this simple tool (in Ruby) I knew I
wanted to structure the interface around subcommands. Git does this to great
effect. If the binary's name is sufficiently short, the operations that it can
perform tend to flow very well and help remind the user what effect they may
have. Thankfully, the most excellent
[CmdArgs](http://hackage.haskell.org/package/cmdargs) package makes subcommands
relatively painless, though not entirely obvious. This post will attempt to
summarize how I patched together a subcommand-driven CLI with CmdArgs. I got
most of this information by scavenging source code off of various and sundry
blog posts. I do not know if this would be considred the best way to go about
solving the problem, but it has worked pretty well for me.

## Project Layout

Everything having to do with the CheckPt code is namespaced to CheckPt. The
Heirarchy looks something like: 

~~~~{.html}
|- CheckPt
|--- CheckPt.CLI (modes, dispatch)
|----- CheckPt.CLI.Mode (Mode(..))
|----- CheckPt.CLI.Add (execute)
|----- CheckPt.CLI.List (execute)
....
~~~~

## Dispatch

CLI exports a list of modes and a dispatch function which takes the result of
CmdArgs parsing and pipes it to dispatch, which pattern matches and routes to
the appropriate module's execute that takes over. If you want to add a new
mode, you add to the mode list, the dispatch and create a new module to handle
it. As you can see from the source, dispatch is pretty simple:

~~~~{.haskell}
dispatch :: Mode -> IO ()
dispatch m = case m of
  Add {}            -> defaultConfig >>= CAdd.execute m
  List {}           -> defaultConfig >>= CList.execute m
  Collection {}     -> defaultConfig >>= CCollection.execute m
  Complete {}       -> defaultConfig >>= CComplete.execute m
  Uncomplete {}     -> defaultConfig >>= CUncomplete.execute m
  Names {}          -> defaultConfig >>= CNames.execute m
  Delete {}         -> defaultConfig >>= CDelete.execute m
  GarbageCollect {} -> defaultConfig >>= CGarbageCollect.execute m
  Init {}           -> defaultConfig >>= CInit.execute m
~~~~


## Mode

The way I use CmdArgs requires me to create an algebraic data type to represent
all of the possible Modes of CheckPt. CmdArg employs
[Data.Typeable](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-Typeable.html)
and other wizardry to generate command flags just from the types and names of
the record fields for the modes you specify, which you can then tweak. Here's
what CheckPt's Mode datatype looks like:

~~~~{.haskell}
data Mode
  = Add            { name :: String, completed :: Bool                  } 
  | List           { rootonly :: Bool                                   } 
  | Collection     { cname :: String , inames :: [String]               } 
  | Complete       { name :: String , inames :: [String], clear :: Bool } 
  | Uncomplete     { name :: String , inames :: [String], clear :: Bool } 
  | Delete         { name :: String , inames :: [String], clear :: Bool } 
  | GarbageCollect {                                                    } 
  | Names          { toplevel :: String                                 }
  | Init           { force :: Bool																			}
  deriving (Show, Typeable, Data)
~~~~

## The Modes Export

The *modes* function that CheckPt.CLI exports is the most important part of
defining the CLI. I essentially pass a list of annotated modes to CmdArgs' *modes_* function to tie it all together like so:

~~~~{.haskell}
import qualified System.Console.CmdArgs as Arg
import           System.Console.CmdArgs((+=),Annotate((:=)),(&=))
modes :: Annotate Arg.Ann
modes  = Arg.modes_  [add,
                      list,
                      collection,
                      complete,
                      uncomplete,
                      names,
                      delete,
                      gc,
                      init]
      += Arg.program "checkpt"
      += Arg.summary "checkpt: track your consumption of media"
      += Arg.help    "Run checkpt help SUBCOMMAND to get more info on one of the subcommands listed below."
  where --... define all the modes below
~~~~

### Boolean and String Flags

Boolean and String flags get handled automagically by CmdArgs. You can even
specify defaults:

~~~~{.haskell}
list = Arg.record List { rootonly = Arg.def }
  [rootonly := False
            += Arg.help "Only list root level items"]
  += Arg.help "Display your list"
  += Arg.auto
~~~~

When passing the record to CmdArgs.record, You're most likely going to want to
assign Arg.def to each filed so your compiler doesn't complain about
unitialized record fields. In the list that follows, you can specify options
for this mode, help text particular to it and default values with :=. Also note
the *Arg.auto* here. This means when someone runs the *checkpt* binary, the
list mode will be invoked by default.

CmdArgs makes available short and longhand flags based on the names of the
fields on the record. In this case, the *list* mode can be invoked via: 

~~~~{.sh}
checkpt list            # List everything
checkpt list -r         # List root-only items
checkpt list --rootonly # Same as above
~~~~

### ARGV Arguments

For string arguments, it is often more convenient to get your argument off of
ARGV, right after the mode. For this, CmdArgs provides the argPos to let you
assign values with the index in ARGV *after* the subcommand. For the *add*
subcommand, I wanted the name to be the next arg. Her'es what the mode
definition looks like:

~~~~{.haskell}
add = Arg.record Add {name = Arg.def, completed = Arg.def}
  [name := error "Must specify a name"
        += Arg.argPos 0
        += Arg.typ "NAME",
  completed := False]
~~~~


There's a few interesting things in the snippet above. Arg.typ allows us to
specify the text placeholder given in the auto-generated help for this
subcommand:

~~~~{.sh}
$> checkpt add --help
checkpt: track your consumption of media

checkpt add [OPTIONS] NAME
  Add a root level item to your list

Flags:
  -c --completed
Common flags:
  -? --help       Display help message
  -V --version    Print version information
~~~~

Note that the default value for name is actually an error. If the user forgets
a name, the program crashes with an error message.

### ARGV List Arguments

For a list of strings, you usually want to specify that you just want to take
the rest of the argument list. I needed this for collection to allow a user to
add a collection and pre-populate it with items. You can combine argPos and
args to achieve this.

~~~~{.haskell}
collection = Arg.record Collection { cname = Arg.def, inames = Arg.def }
  [cname := error "Must specify a name"
        += Arg.argPos 0
        += Arg.typ "NAME",
  inames := []
        += Arg.args
        += Arg.typ "ITEM_NAMES"]
  += Arg.help "Add or list a collection"
~~~~

This allows you to call this mode like so:
    
~~~~{.sh}
checkpt collection foos
checkpt collection foos bar baz
~~~~

### Rename Subcommand

CmdArgs will attempt to name your subcommand based on the name of the record.
garbagecollect is a lot more tedious to type than gc so i use "name" to
explicitly indicate the subcommand.

~~~~{.haskell}
gc = Arg.record GarbageCollect { }
  []
  += Arg.name "gc"
  += Arg.help "Delete completed line items and collections"
~~~~

## Main

The main for a program structured like this becomes a dead-simple one-liner:

~~~~{.haskell}
import CheckPt.CLI (modes, dispatch)

import qualified System.Console.CmdArgs as Arg

main :: IO ()
main = Arg.cmdArgs_ modes >>= dispatch
~~~~

## Conclusion

I had quite a bit of difficulty figuring out CmdArgs at first, and I'm
definitely not using it to its full potential, but this handles my use case
quite well. A lot of the design that went into CheckPt's CLI is thanks to
various blog posts which helped me understand how CmdArgs works where the
documentation came up short. Hopefully this post will be a useful resource to
someone who wants to create a first-rate CLI in Haskell. Check out CmdArgs'
[documentation](http://hackage.haskell.org/packages/archive/cmdargs/0.6.8/doc/html/System-Console-CmdArgs.html)
for more information. Thanks to [Neil Mitchell](http://neilmitchell.blogspot.com/) 
for creating such a kickass library.
