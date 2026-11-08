# AGENTS.md

This project maintains a set of Haskell packages for openSUSE Tumbleweed. The
packages are generated programmatically from a downloaded copy of the Hackage
database located at `~/.cabal/packages/hackage.haskell.org/01-index.tar`. This
is accomplished by executing the command:

    cabal run cabal2obs

If successful, this populates the `_build` directory with one package set per
supported compiler. Currently, the compiler we use is ghc-9.12.x. Inside of
that directory, e.g. `_build/ghc-9.12.x` are the available packages -- one per
directory. Those packages are OBS builds and they are checked into the public
OBS project

    https://build.opensuse.org/project/show/devel:languages:haskell:ghc-9.12.x

from where they are propagated into the openSUSE Tumbleweed distribution
automatically at regular intervals.

To rebuild any specific package, say `ghc-tls`, run:

    cabal run cabal2obs -- _build/ghc-9.12.x/ghc-tls/ghc-tls.spec

To compile any individual package, run `osc build` in its generated package
directory. This works, however, only on hosts that have `/usr/bin/build`. Hosts
that lack this tool can use the `tw-dev` connection to run builds instead. The
machine shares the project disk with the local host, so absolute project paths
are identical on both machines; no source synchronization is needed.
