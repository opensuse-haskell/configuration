# GNUmakefile

.PHONY: all cabal-rpm cabal2obs update

all:		config/lts-6/stackage-packages.txt
all:		config/lts-7/stackage-packages.txt
all:		config/nightly/stackage-packages.txt
all:		cabal-rpm cabal2obs
	nice -n20 tools/cabal2obs/dist/build/cabal2obs/cabal2obs -j$$(nproc) --lint -V

cabal-rpm:
	@cd tools/cabal-rpm && cabal build

cabal2obs:
	@cd tools/cabal2obs && hpack && cabal build

config/%/stackage-packages.txt:
	curl -L -s "https://www.stackage.org/$*/cabal.config" >$@

update:
	cabal update
	@cd hackage && git pull
	f=$$(ls config/*/stackage-packages.txt); rm $$f; $(MAKE) $$f
