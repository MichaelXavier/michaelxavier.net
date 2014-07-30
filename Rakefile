task :default => :build

desc "Create a post. Set the TITLE env var"
task :post do
  require 'fileutils'

  raw_title = ENV.fetch('TITLE')
  title     = raw_title.gsub(/[^\w]/, '-').squeeze('-')
  filename  = "posts/#{Time.now.strftime('%Y-%m-%d')}-#{title}.markdown"
  File.open(filename, 'w') {|f| f.puts(<<-EOF)}
---
title: #{raw_title}
categories: 
---
EOF
  exec("#{ENV.fetch('EDITOR')} #{filename}")
end

desc "Build the executable"
task :mxnet => "mxnet.hs" do
  sh "cabal build"
end

desc "preview the site"
task :preview  do
  sh "cabal run watch"
end

desc "rebuild the site"
task :build do
  sh "cabal run build"
end

desc "clean up generated site"
task :clean do
  sh "cabal run clean"
end

desc "upload generated site"
task :upload => :build do
  chdir '_site' do
    sh 'find * -type f -print | s3funnel michaelxavier.net PUT -t 4 --put-full-path -v'
  end
end
