---
title: Snippet: Rack::CommonLogger in Sinatra
categories: ruby,sinatra,code snippet
---

It's been a little while. I've been completely swamped with school and work, but I realized there wasn't really any code samples I could find for using Rack::CommonLogger in Sinatra. Solution after the jump.

If you wanted to log stuff like you would in rails and sinatra, here would be a very clean way to do it.

    #!sh_ruby
    configure do
      Dir.mkdir('log') unless File.exists?('log')
    end

    configure :test do
      use Rack::CommonLogger, File.new('log/test.log', 'w')
    end

    configure :development do
      use Rack::CommonLogger, File.new('log/development.log', 'w')
    end

    configure :production do
      use Rack::CommonLogger, File.new('log/production.log', 'w')
    end

More updates to come once I get out of midterm hell next week.