all: build

mxnet: mxnet.hs 
	ghc --make mxnet.hs

preview: mxnet
	./mxnet preview

build: mxnet css/* files/* images/* javascripts/* posts/*
	./mxnet rebuild

clean:
	rm -rf _site _cache mxnet mxnet.hi

upload: build
	rsync -aP -e ssh _site/ mxn:"~/public_html/"