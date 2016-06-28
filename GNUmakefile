# GNUmakefile

.PHONY: all

include config.mk

config.mk : generate.hs cabal.config
	runhaskell generate.hs >$@
