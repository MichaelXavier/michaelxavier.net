---
title: Depaginating Web Resources with Enumerators
categories: haskell, googleplus
---

## Depagination Defined
Depagination is a word I probably made up. When working on
[Web.GooglePlus](http://hackage.haskell.org/package/googleplus) I noticed that
a lot of resources exposed in the Google+ API in their response return a "page
token" to the next page, like so:

~~~{.json}
{
  "kind": "plus#peopleFeed",
  "selfLink": string,
  "title": string,
  "nextPageToken": string,
  "items": [
    people Resource
  ]
}
~~~

Pagination is nice for an end user but it is typically not very useful to the
user of a library. If I want to perform a search for people on Google+ with
Web.GooglePlus, my application's code shouldn't have to carry the burden of
traversing pages of data. It should consume as many results as it needs until
the results run out or some applicaiton logic decides it has had enough. That's
where depagination with Enumerators comes in.

## Enter unfoldM

The trick to depagination where the next page token is included alongside the
current page is that you must maintain state to know when to stop enumerating
and to have the token to get the next page.

Data.Enumerator.List provides [unfoldM](http://hackage.haskell.org/packages/archive/enumerator/latest/doc/html/Data-Enumerator-List.html#v:unfoldM) which has the type:

~~~{.haskell}
unfoldM :: Monad m => (s -> m (Maybe (a, s))) -> s -> Enumerator a m b
~~~

unfoldM takes a function which, given a state, either produces a result and the
modified state or returns Nothing, thus terminating the Enumerator's stream.

For my specific case, unfoldM wasn't *quite* what I needed. It would yield a
list of results at a time as a single
[Chunk](http://hackage.haskell.org/packages/archive/enumerator/latest/doc/html/Data-Enumerator.html#v:Chunks)
per page. Instead, it would make more sense for each chunk to be a list of
results, say [Person] or [Activity] in terms of Google+. For this, I needed to
make a slight modification to unfoldM:

~~~{.haskell}
-- Exactly the same as unfoldM but takes the result of the stateful function
-- and uses it as the chunks, rather than a Chunks with a singleton list
unfoldListM :: Monad m => (s -> m (Maybe ([a], s)))
                          -> s
                          -> Enumerator a m b
unfoldListM f = checkContinue1 $ \loop s k -> do
	fs <- lift (f s)
	case fs of
		Nothing -> continue k
		Just (as, s') -> k (Chunks as) >>== loop s'
~~~

With that out of the way, I was able to create a generic depaginator for any
paginated resource in the Google+ API. First, the relevant types:

~~~{.haskell}
type PageToken             = Text
data DepaginationState     = FirstPage |
                             MorePages PageToken |
                             NoMorePages

simpleDepaginator  :: Monad m => (DepaginationState -> m (Maybe ([a], DepaginationState)))
                                 -> Enumerator a m b
simpleDepaginator depaginate = unfoldListM depaginate FirstPage
~~~

simpleDepaginator sets up the depagination by initializing the state to being
on the first page. The first page is a special case where there is no previous
page token. It takes the step as an argument which fetches the next page and
mutates the state. Here's what the generic depagination step looks like:

~~~{.haskell}
paginatedState :: (a, Maybe PageToken)
                  -> (a, DepaginationState)
paginatedState (results, token) = (results, maybe NoMorePages MorePages token)

simpleDepaginationStep :: FromJSON a => Integer
                                     -> Ascii 
                                     -> Query 
                                     -> DepaginationState 
                                     -> GooglePlusM (Maybe ([a], DepaginationState))
simpleDepaginationStep perPage pth params FirstPage       = (return . fmap paginatedState) =<< simpleGetFirstPage perPage pth params
simpleDepaginationStep perPage pth params (MorePages tok) = (return . fmap paginatedState) =<< simpleGetPage perPage (Just tok) pth params
simpleDepaginationStep _ _ _ NoMorePages = return Nothing
~~~

Simple as can be. All 3 cases of the state are handled. In the first two, there
is more data to get and thus an HTTP GET is performed. If we are on page 2 or
higher, it includes the token. If there are no more pages, the depagination
step returns Nothing, which terminates the stream of data.

paginatedState simply looks at the presence or absence of the current request's
page token to determine if the enumerator should continue requesting pages or
not.

## Cost/Benefit of this approach
The process of coming up with this abstraction results in a nice way to yield
results of the API calls before having gotten all the pages. This is the
inherant benefit of designing for the Enumerator interface. The application
using the library does not necessarily have to hold all results in memory, nor
wait for them all to be fetched to deal with them, nor concern itself with the
intricacies of paginating the requests. 

Also, using enumerators makes defining non-enumerator interfaces dead simple. You just consume the enumerator:

~~~{.haskell}
import qualified Data.Enumerator.List as EL

enumActivities :: PersonID              -- ^ Feed owner ID
                  -> ActivityCollection -- ^ Indicates what type of feed to retrieve
                  -> Maybe Integer      -- ^ Page size. Should be between 1 and 100. Defualt 20
                  -> Enumerator Activity GooglePlusM b
enumActivities pid coll perPage = simpleDepaginator depaginate
  where depaginate = -- ...


getActivities :: PersonID              -- ^ Feed owner ID
                 -> ActivityCollection -- ^ Indicates what type of feed to retrieve
                 -> GooglePlusM [Activity]
getActivities pid coll = run_ $ enumActivities pid coll (Just 100) $$ EL.consume
~~~

getActivities will then consume pages of 100 activities at a time (the maximum)
and will not yield a result until it hits the end. If the user wants to do
something quick and dirty, just slurping the entire resource without having to
worry about enumerators is an attractive option.


The main fault I can see with using unfoldM is that it can be lossy and can
send some false signals if you use it on an API which may change its data
format at any time. For instance, I've coded my datatypes against the Google+
specs and I'm fairly confident I've gotten all the fields right. This includes
handling fields which may possibly be absent such as email addresses, URLs,
etc. However, because unfoldM is terminated with a Nothing, a fatal parse error
to the consumer is *indistinguishable* from hitting the last page, it
terminates the stream and says nothing more. If the resource you are consuming
is more error prone than most, it may be a good idea to roll your own unfoldM
which uses Either to distinguish between normal termination and termination due
to error.

Overall I really like this pattern and will probably use it on any future web
API projects I do that require consuming a paginated resource.
