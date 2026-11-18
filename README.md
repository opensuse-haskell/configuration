# Haskell for openSUSE

To generate the Haskell OBS repositories for
[openSUSE Linux](http://opensuse.org/), perform the following steps:

1. Clone this repository with the `--recursive` flag or, alternatively, run

        $ git submodule update --init

   in your checked-out copy.

2. Check out the
   [devel:languages:haskell:ghc-9.14.x](https://build.opensuse.org/project/show/devel:languages:haskell:ghc-9.14.x)
   OBS repository in a `_build/` sub-directory by running:

        $ mkdir -p _build
        $ osc co devel:languages:haskell:ghc-9.14.x -o _build/ghc-9.14.x

3. Run `cabal update`.

4. Execute `cabal run -- cabal2obs` to re-generate all spec files. Note that
   the initial run of the build system might take a while. Once the initial build
   has succeeded, further re-runs will be very fast. Run `cabal run -- cabal2obs -B`
   to force a re-generation of all spec files, e.g. after a change to cabal2spec.

5. Inspect the `_build/ghc-9.14.x` hierarchy with `osc status` and `osc diff` to
   make sure that all generated changes look reasonable. There should be no
   modifications in any of these manually maintained packages:

   - ghc
   - ghc-bootstrap
   - ghc-bootstrap-helpers
   - ghc-rpm-macros
   - hadrian

6. Commit:

        $ pushd _build/ghc-9.14.x && ../../tools/commit && popd
