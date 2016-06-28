# GNUmakefile

OBSDIR := obs

.PHONY: all

include config.mk

config.mk : generate.hs cabal.config
	runhaskell generate.hs >$@

cabal.config:
	wget -q https://www.stackage.org/lts/cabal.config
