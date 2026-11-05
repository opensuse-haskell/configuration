# AGENTS.md

This project maintains a set of Haskell packages for use in openSUSE
Tumbleweed. The packages are generated programmatically from a downloaded copy
of the Hackage database. This is accomplished by executing the command:

    cabal run cabal2obs

If successful, this populates the `_build` directory with one package set per
supported compiler. Currently, the compiler we use is ghc-9.12.x. Inside of
that directory, e.g. `_build/ghc-9.12.x` are the available packages -- one per
directory. Those packages are OBS builds and they are checked into the public
OBS project `devel:languages:haskell:ghc-9.12.x` from where they are propagated
into Tumbleweed automatically at regular intervals.
