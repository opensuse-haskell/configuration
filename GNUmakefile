# GNUmakefile

CABAL_INSTALL_TARBALL := $(HOME)/.cabal/packages/hackage.haskell.org/00-index.tar

.PHONY: all cabal2obs update

all:		$(PACKAGE_SETS)
all:		cabal2obs
	rm -f $(CABAL_INSTALL_TARBALL)*
	cd hackage && git archive --format=tar -o $(CABAL_INSTALL_TARBALL) HEAD
	gzip <$(CABAL_INSTALL_TARBALL) >$(CABAL_INSTALL_TARBALL).gz
	cabal fetch -v0 --no-dependencies ip6addr
	nice -n20 tools/cabal2obs/dist/build/cabal2obs/cabal2obs -j$$(nproc) --lint -V $(CABAL2OBS_FLAGS)

cabal2obs:
	@cd tools/cabal2obs && hpack && cabal build

update:		# TODO: move this code into cabal2obs
	cd hackage && git checkout hackage && git pull
