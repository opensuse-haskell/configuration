# Haskell for openSUSE

To generate the Haskell OBS repositories for
[openSUSE Linux](http://opensuse.org/), perform the following steps:

1. Clone this repository with the `--recursive` flag or, alternatively, run

        $ git submodule update --init

   in your checked-out copy.

2. Check out the
   [devel:languages:haskell:ghc-8.10.x](https://build.opensuse.org/project/show/devel:languages:haskell:ghc-8.10.x) and
   [devel:languages:haskell:ghc-9.2.x](https://build.opensuse.org/project/show/devel:languages:haskell:ghc-9.2.x) and
   OBS repositories in a `_build/` sub-directory by running:

        $ mkdir -p _build
        $ osc co devel:languages:haskell:ghc-8.10.x -o _build/ghc-8.10.x
        $ osc co devel:languages:haskell:ghc-9.2.x -o _build/ghc-9.2.x

4. Run `cabal update`.

5. Execute `cabal run -- cabal2obs` to re-generate all spec files. Note that
   the initial run of the build system might take a while. Once the initial build
   has succeeded, further re-runs will be very fast.

6. Inspect the `build/ghc-*` hierarchies with `osc status` and `osc diff` to
   make sure that all generated changes look reasonable. There should be no
   modifications in any of these projects:

   - ghc
   - ghc-bootstrap
   - ghc-rpm-macros

8. Commit:

        $ pushd _build/ghc-8.10.x && ../../tools/commit && popd
        $ pushd _build/ghc-9.2.x && ../../tools/commit && popd
