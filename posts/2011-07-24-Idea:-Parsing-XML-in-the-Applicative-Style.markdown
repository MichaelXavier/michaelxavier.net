---
title: Idea: Parsing XML in the Applicative Style
categories: haskell, xml
---

Motivation
----------

I've recently undertaken a project to implement a Haskell library for the [MusicBrainz XML Web Service](http://musicbrainz.org/doc/XML_Web_Service/Version_2). MusicBrainz is an open-source alternative to the CDDB service. Their aim is to catalog a vast amount of data about musical artists, releases, labels, etc.

My first task in starting this project was to find out how to parse XML. I would greatly have preferred the service have a JSON option so that I could use [Aeson](http://github.com/mailrank/aeson), but alas, XML was the only option.

After looking at HXT and others, I was drawn to the simple API of [xml-enumerator](http://hackage.haskell.org/package/xml-enumerator) by the prolific [Michael Snoyman](http://www.snoyman.com).

Having some experience with Aeson, I know that I would tend to parse data in the applicative style by creating an instance of FromJSON like so:

~~~~{.haskell}
data User = User { userId           :: Id,
                   userName         :: T.Text,
                   userEmailAddress :: T.Text }
            deriving (Eq, Ord, Read, Show, Typeable)

instance FromJSON User where
  parseJSON (Object v) = User <$> v .: "id"
                              <*> v .: "name"
                              <*> v .: "email_address"
  parseJSON _          = mzero
~~~~

The advantage to this style is that it is very compact and reads (to me at least) like a declaration of format rather than a procedure to parse, which makes it much easier to grok and maintain. I found that there was nothing built in to xml-enumerator that would do this so I set out to define my XML parsing code in this style, regardless of how long it took (I'm still a very slow Haskeller at this point).

Ingredients Needed from the xml-enumerator API
----------------------------------------------

Coming from a Ruby background, I'm used to parsing XML with [Nokogiri](http://nokogiri.org). In that library, the basic element you work with most is a Node. It represents a particular XML node within the document from which you can traverse up and down the document tree. In xml-enumerator, the best analog I found was [Text.XML.Enumerator.Cursor](http://hackage.haskell.org/packages/archive/xml-enumerator/0.3.4/doc/html/Text-XML-Enumerator-Cursor.html). This module leverages the concepts of Axes and Cursors to move around the XML document. A cursor is just a type synonym:

~~~~{.haskell}
type Axis = Cursor -> [Cursor]
~~~~

The Cursor module also provides a lot of helpful infix operators with fairly logical symbols, such as &|, &/, &//, $|, $/, and $//. The & represents combining axes whereas the $ represents applying axes to cursors. The |, / and // operates on the cursor itself, children, and descendants of the result of the cursor/results respectively. 

I also lifted some exception handling code from another library that uses xml-enumerator:

~~~~{.haskell}
import qualified Text.XML.Enumerator.Cursor as Cu
forceEx :: F.Failure XmlException m => String -> [a] -> m a
forceEx = Cu.force . XmlException
~~~~

This takes the result of an axis in our case and raises an exception if it is empty or returns the first result if it is not.


Writing Infix Operators of My Own
---------------------------------
In order to a achieve the most basic XML document traversal abitlies, I created a suite of infix operators with the convention that the left side would take a cursor on which to operate and the right side would take Text referring to tag or attribute names.

The first set of operators are used for extracting attributes from the given node:

~~~~{.haskell}
(!<@>) :: F.Failure XmlException m => Cu.Cursor -> T.Text -> m T.Text
el !<@> n = forceEx ("missing " ++ T.unpack n) $ el $| laxAttribute n

(?<@>) :: Cu.Cursor -> T.Text -> Maybe T.Text
el ?<@> n =  listToMaybe $ el $| laxAttribute n
~~~~

laxAttribute extracts the attribute from a cursor, ignoring case, which is almost always what you want. ! as a prefix indicates that you expect the attribute to be there and that an exception should be raised otherwise. ? is less strict and just returns a Maybe.

Inside of the angle brackets of the operator, I have several symbols:

**<@>**
:    Retrieves the attribute from the element

**<.>**

:    Retrieves a cursor pointing to the specified child element. Example:

        el !<.> "some-child-tag"

**<|>**
:    Retrieves the content from the element.

**<=>**
:    Passes the element through fromXML, which parses it into a record.

**<\/\/.>, <\/\/|>, and <\/\/=>**

:    Retrieves the cursor, content, and parsed record from a specified path through the document tree, respectively.

        el !<//=> ["artist-list", "artist"]

The syntax seems a bit obtuse at first but it gets to be natural. I'm thinking I might break it out into a separage package from Musicbrainz.

The FromXML Instance
--------------------
Here's where the code really shines. Like in Aeson, I wrote a typeclass to describe all types which could be deserialized from XML:

~~~~{.haskell}
import qualified Control.Failure as F

class FromXML a where
  fromXML :: (Functor m, Applicative m, F.Failure XmlException m) => Cu.Cursor -> m a
~~~~

fromXML requires that you pass in the Cursor corresponding to the datatype you are parsing. This makes it really nice to parse types that are composed of other types with an established XML structure.

Tying It All Together with Applicative
--------------------------------------
The final product can be tied together fairly concisely with the applicative style. It gets uglied up a little bit if you're parsing data where some fields are in the XmlException monad and some aren't (thus necessitating the use of pure), but this issue is minor. Here's a simplified example from my MusicBrainz library:

~~~~{.haskell}
data Artist = Artist { artistId             :: Text,
                       artistType           :: Maybe Text,
                       artistName           :: Text,
                       artistAliases        :: [Text],
                       artistRecordings     :: [Recording] } deriving (Show, Eq)

fromXML el = Artist <$>                               el !<@> "id"
                    <*> (pure $                       el ?<@> "type")
                    <*>                               el !<|> "name"
                    <*> (pure $                       el !<//|> ["alias-list", "alias"])
                    <*>                               el <//=> ["recording-list", "recording"]
~~~~

In my opinion, this style of parsing XML is fairly readable and gets rid of a lot of line noise that you would incur otherwise. I plan on parsing XML like this in any future libraries I write, provided I don't come across anything better. There are certainly times to use HXT or the lower-level aspects of xml-enumerator, but for scraping data out of well-defined XML documents and mapping them up to Haskell records, it's hard to bet applicative.
