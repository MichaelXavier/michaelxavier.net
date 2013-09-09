all: build

post:
	$(EDITOR) posts/`date +%Y-%m-%d`-`echo '$(TITLE)' | sed 's/[ :,]/-/g'`.markdown

mxnet: mxnet.hs 
	ghc --make mxnet.hs -package-db .cabal-sandbox/x86_64-linux-ghc-7.6.3-packages.conf.d

preview: mxnet
	./mxnet preview

build: mxnet css/* files/* images/* javascripts/* posts/*
	./mxnet rebuild

clean:
	rm -rf _site _cache mxnet mxnet.hi

upload: build
	rsync -aP -e ssh _site/ mxn:"~/public_html/"
