# GNUmakefile

VERSION := lts-6

OBSDIR := $(VERSION)/_obs

.PHONY: all

include $(VERSION)/_obs.mk

$(VERSION)/_obs.mk : tools/cabal2obs/Main.hs $(VERSION)/_obs.config $(HOME)/.cabal/packages/hackage.haskell.org/00-index.tar
	runhaskell tools/cabal2obs/Main.hs $(VERSION)/_obs.config >$@

$(VERSION)/_obs.config:
	curl -L -s https://www.stackage.org/$(VERSION)/cabal.config >$@
