# GNUmakefile

STACKAGE_VERSION = lts

all::  config.mk

cabal.config:
	curl >$@ -L "http://www.stackage.org/$(STACKAGE_VERSION)/cabal.config"

package-list.txt : cabal.config
	sed <$< >$@ -e "/^--/d" -e "s/^constraints://" -e "s/ *//g" -e "s/,//" -e "s/==/-/"

config.mk: package-list.txt
	for n in $$(cat package-list.txt); do \
	  echo $$n; \
	done

clean::
	rm -f config.mk package-list.txt
