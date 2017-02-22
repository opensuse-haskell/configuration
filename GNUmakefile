# GNUmakefile

CABAL_INSTALL_TARBALL := $(HOME)/.cabal/packages/hackage.haskell.org/00-index.tar
PACKAGE_SETS := $(wildcard config/*/stackage-packages.txt)

.PHONY: all cabal-rpm cabal2obs update

all:		$(PACKAGE_SETS)
all:		cabal-rpm cabal2obs
	rm -f $(CABAL_INSTALL_TARBALL)*
	cd hackage && git archive --format=tar -o $(CABAL_INSTALL_TARBALL) HEAD
	gzip <$(CABAL_INSTALL_TARBALL) >$(CABAL_INSTALL_TARBALL).gz
	cabal fetch -v0 --no-dependencies ip6addr
	nice -n20 tools/cabal2obs/dist/build/cabal2obs/cabal2obs -j$$(nproc) --lint -V $(CABAL2OBS_FLAGS)

cabal-rpm:
	@cd tools/cabal-rpm && cabal build

cabal2obs:
	@cd tools/cabal2obs && hpack && cabal build

config/%/stackage-packages.txt:
	curl -L -s "https://www.stackage.org/$*/cabal.config" >$@

update:
	cd hackage && git checkout hackage && git pull
	rm -f $(PACKAGE_SETS)
	$(MAKE) $(PACKAGE_SETS)
