# stackage2obs

This repository provides tools to (semi-)automatically create, update, and
maintain OBS projects that mirror a given Stackage release. Just run

    $ cabal update
    $ gmake -j -l$(nproc)

to set up a repository. The build requires:

- A recent version of GHC.
- The [http://hackage.haskell.org/package/hackage-db](hackage-db) library.
- [http://hackage.haskell.org/package/cabal-rpm](cabal-rpm)
- [http://hackage.haskell.org/package/cabal-install](cabal-install)
