# GNUmakefile

VERSION := lts-6

OBSDIR := obs-$(VERSION)

.PHONY: all

include $(VERSION).mk

$(VERSION).mk : generate.hs cabal-$(VERSION).config $(HOME)/.cabal/packages/hackage.haskell.org/00-index.tar
	runhaskell generate.hs cabal-$(VERSION).config >$@

cabal-$(VERSION).config:
	curl -L -s https://www.stackage.org/$(VERSION)/cabal.config >$@
