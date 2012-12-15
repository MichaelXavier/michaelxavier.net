---
title: Padding Incomplete XML in Ruby
categories: ruby, xml, tips
---

I was recently given the task of parsing and processing a huge XML file of
data. The format looks something like:

~~~{.xml}
<release id="1">...<release>
<release id="2">...<release>
<release id="3">...<release>
~~~

After some experimentation, I decided that Nokgori's
[XML::Reader](http://nokogiri.org/Nokogiri/XML/Reader.html). The standard
Nokogiri::XML parser was out because the files are approximately 1GB in size.
SAX was out because it requires you to basically build your own state machine
and piece together the parsed nodes from events. I don't know about you but the
last thing I want to do is do a bunch of work to parse XML. XML::Reader can
incrementally parse a stream and iterate through each parsed node, which is
perfect.

The problem is that XML does not support multiple root nodes. If you run this
through the standard Nokogiri parser, it will just parse the first node and
stop. We need a way to pad the XML with an enclosing tag, say "releases". But
we also want to use an IO stream rather than a full string since the file is
upwards of 1gb.

## Enter filter_io
There is a great gem called [filter_io](http://rubygems.org/gems/filter_io)
that fits just the bill. filter_io wraps an IO object and gives you a stateful
stream modifier. We're interested in attaching 2 transformations to the stream:

1. The beginning of the stream needs an opening tag.
2. The end of the stream needs a closing tag.

Here's the class I came up with to wrap the IO stream:

~~~{.ruby}
require 'filter_io'

class RootTagWrapper
  attr_reader :root_tag_name

  def initialize(root_tag_name)
    @root_tag_name = root_tag_name
  end

  def wrap(io)
    FilterIO.new(io) do |data, state|
      if state.bof?
        open_tag + data
      elsif state.eof?
        data + close_tag
      else
        data
      end
    end
  end

private
  def open_tag
    "<#{root_tag_name}>"
  end

  def close_tag
    "</#{root_tag_name}>"
  end
end
~~~

Here is a usage example:

~~~{.ruby}
io = RootTagWrapper.new('releases').wrap(ARGF)
Nokogiri:XML(io).each do |node|
  if node.name == 'release'
    yield Nokogiri::XML.fragment(node.outer_xml)
  end
end
~~~
