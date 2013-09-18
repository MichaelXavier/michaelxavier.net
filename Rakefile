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
	sh "ghc --make mxnet.hs -package-db .cabal-sandbox/x86_64-linux-ghc-7.6.3-packages.conf.d"
end

desc "preview the site"
task :preview => :mxnet do
  sh "./mxnet preview"
end

desc "rebuild the site"
task :build => [:mxnet, *FileList.new('css/*',
                                      'files/*',
                                      'images/*',
                                      'assets/*',
                                      'javascripts/*',
                                      'posts/*',
                                      'templates/*')] do
  sh "./mxnet build"
end

desc "clean up generated site"
task :clean => :mxnet do
  sh "./mxnet clean"
end

desc "upload generated site"
task :upload => :build do
 sh 'rsync -aP -e ssh _site/ mxn:"~/public_html/"'
end
