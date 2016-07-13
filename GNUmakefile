# GNUmakefile

.PHONY: all cabal-rpm cabal2obs update

all:		lts-6/config/stackage-packages.txt
all:		nightly/config/stackage-packages.txt
all:		cabal-rpm cabal2obs
	env PATH=$(PWD)/tools/cabal-rpm/dist/build/cabal-rpm:$$PATH tools/cabal2obs/dist/build/cabal2obs/cabal2obs -j$$(nproc)

cabal-rpm:
	@cd tools/cabal-rpm && cabal build

cabal2obs:
	@cd tools/cabal2obs && cabal build

%/config/stackage-packages.txt:
	curl -L -s "https://www.stackage.org/$*/cabal.config" >$@

update:
	f=$$(ls */config/stackage-packages.txt); rm $$f; $(MAKE) $$f
	cabal update
