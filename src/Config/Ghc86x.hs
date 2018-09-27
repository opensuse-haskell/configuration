{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Config.Ghc86x ( ghc86x ) where

import Config.ForcedExecutables
import Oracle.Hackage ( )
import Types

import Control.Monad
import Data.Map.Strict as Map ( fromList, toList )
import Data.Maybe
import Development.Shake
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Simple.Utils ( lowercase )
import Distribution.Text

ghc86x :: Action PackageSetConfig
ghc86x = do
  let compiler = "ghc-8.6.1"
      flagAssignments = fromList (readFlagAssignents flagList)
      forcedExectables = forcedExectableNames
  packageSet <- fromList <$>
                  forM (toList constraintList) (\(pn,vr) ->
                    (,) pn <$> askOracle (Dependency pn vr))
  pure (PackageSetConfig {..})

targetPackages :: ConstraintSet
targetPackages   = [ "alex >=3.2.4"
                   , "cabal-install >=2.2.0.0"
                   , "cabal2spec >2.2"
                   , "distribution-opensuse >1"
                   , "happy >=1.19.9"
                   , "pandoc >=2.2.1"
                   , "ShellCheck >=0.4.7"
                   , "stack >=1.7.1"
                   , "xmobar >= 0.27"
                   , "xmonad >= 0.14"
                   , "xmonad-contrib >= 0.14"
                   , "SDL >=0.6.6.0"        -- Dmitriy Perlow <dap.darkness@gmail.com> needs the
                   , "SDL-image >=0.6.1.2"  -- SDL packages for games/raincat.
                   , "SDL-mixer >=0.6.2.0"
                   ]

constraintList :: ConstraintSet
constraintList = [ "aeson == 1.4.1.0"
                 , "alex == 3.2.4"
                 , "ansi-terminal == 0.8.1"
                 , "ansi-wl-pprint == 0.6.8.2"
                 , "async == 2.2.1"
                 , "attoparsec == 0.13.2.2"
                 , "base-compat == 0.10.4"
                 , "base-orphans == 0.8"
                 , "base-prelude == 1.3"
                 , "base16-bytestring == 0.1.1.6"
                 , "base64-bytestring == 1.0.0.1"
                 , "bifunctors == 5.5.3"
                 , "cabal-doctest == 1.0.6"
                 , "cabal-install == 2.4.0.0"
                 , "cabal2spec == 2.2.2"
                 , "colour == 2.3.4"
                 , "comonad == 5.0.4"
                 , "extra == 1.6.12"
                 , "contravariant == 1.5"
                 , "cryptohash-sha256 == 0.11.101.0"
                 , "data-default == 0.7.1.1"
                 , "data-default-class == 0.1.2.0"
                 , "data-default-instances-containers == 0.0.1"
                 , "data-default-instances-dlist == 0.0.1"
                 , "data-default-instances-old-locale == 0.0.1"
                 , "Diff == 0.3.4"
                 , "digest == 0.0.1.2"
                 , "distribution-opensuse == 1.1.1"
                 , "distributive == 0.6"
                 , "dlist == 0.8.0.5"
                 , "echo == 0.1.3"
                 , "ed25519 == 0.0.5.0"
                 , "edit-distance == 0.2.2.1"
                 , "extensible-exceptions == 0.1.1.4"
                 , "extra == 1.6.12"
                 , "foldl == 1.4.4"
                 , "hackage-security == 0.5.3.0"
                 , "happy == 1.19.9"
                 , "hashable == 1.2.7.0"
                 , "hostname == 1.0"
                 , "hsc2hs == 0.68.4"
                 , "hsemail == 2"
                 , "HTTP == 4000.3.12"
                 , "integer-logarithms == 1.0.2.2"
                 , "managed == 1.0.6"
                 , "math-functions == 0.3.0.2"
                 , "X11-xft == 0.3.1"
                 , "mwc-random == 0.14.0.0"
                 , "network == 2.7.0.2"
                 , "network-uri == 2.6.1.0"
                 , "old-locale == 1.0.0.7"
                 , "old-time == 1.1.0.3"
                 , "optional-args == 1.0.2"
                 , "optparse-applicative == 0.14.2.0"
                 , "parsec-class == 1.0.0.0"
                 , "primitive == 0.6.4.0"
                 , "profunctors == 5.3"
                 , "random == 1.1"
                 , "resolv == 0.1.1.1"
                 , "scientific == 0.3.6.2"
                 , "SDL == 0.6.7.0"
                 , "SDL-image == 0.6.2.0"
                 , "SDL-mixer == 0.6.3.0"
                 , "semigroupoids == 5.3.1"
                 , "semigroups == 0.18.5"
                 , "setlocale == 1.0.0.8"
                 , "StateVar == 1.1.1.1"
                 , "system-fileio == 0.3.16.4"
                 , "system-filepath == 0.4.14"
                 , "tagged == 0.8.6"
                 , "tar == 0.5.1.0"
                 , "temporary == 1.3"
                 , "th-abstraction == 0.2.8.0"
                 , "time-locale-compat == 0.1.1.5"
                 , "transformers-compat == 0.6.2"
                 , "turtle == 1.5.11"
                 , "unix-compat == 0.5.1"
                 , "unordered-containers == 0.2.9.0"
                 , "utf8-string == 1.0.1.1"
                 , "uuid-types == 1.0.3"
                 , "vector == 0.12.0.1"
                 , "vector-builder == 0.3.6"
                 , "vector-th-unbox == 0.2.1.6"
                 , "X11 == 1.9"
                 , "X11-xft == 0.3.1"
                 , "xmonad == 0.14.2"
                 , "xmonad-contrib == 0.14"
                 , "zip-archive == 0.3.3"
                 , "zlib == 0.6.2"
                 ]

flagList :: [(String,String)]
flagList =
  [ -- Don't build hardware-specific optimizations into the binary based on what the
    -- build machine supports or doesn't support.
    ("cryptonite",                     "-support_aesni -support_rdrand -support_blake2_sse")

    -- Don't use the bundled sqlite3 library.
  , ("direct-sqlite",                  "systemlib")

    -- Build the standalone executable and prefer pcre-light, which uses the system
    -- library rather than a bundled copy.
  , ("highlighting-kate",              "executable pcre-light")

    -- Don't use the bundled sass library.
  , ("hlibsass",                       "externalLibsass")

    -- Use the bundled lua library. People expect this package to provide LUA
    -- 5.3, but we don't have that yet in openSUSE.
  , ("hslua",                          "-system-lua")

    -- Allow compilation without having Nix installed.
  , ("nix-paths",                      "allow-relative-paths")

    -- Build the standalone executable.
  , ("texmath",                        "executable")

    -- Enable almost all extensions.
  , ("xmobar",                         "with_thread with_utf8 with_xft with_xpm with_mpris with_dbus with_iwlib with_inotify")

    -- Enable additional features
  , ("idris",                          "ffi gmp")

    -- Disable dependencies we don't have.
  , ("invertible",                     "-hlist -piso")

    -- Since version 6.20170925, the test suite can no longer be run outside of
    -- a checked-out copy of the git repository.
  , ("git-annex",                      "-testsuite")

    -- Compile against the system library, not the one bundled in the Haskell
    -- source tarball.
  , ("cmark",                          "pkgconfig")

    -- Fix build with modern compilers.
  , ("cassava",                        "-bytestring--lt-0_10_4")
  ]

readFlagAssignents :: [(String,String)] -> [(PackageName,FlagAssignment)]
readFlagAssignents xs = [ (fromJust (simpleParse name), readFlagList (words assignments)) | (name,assignments) <- xs ]

readFlagList :: [String] -> FlagAssignment
readFlagList = mkFlagAssignment . map (tagWithValue . noMinusF)
  where
    tagWithValue ('-':fname) = (mkFlagName (lowercase fname), False)
    tagWithValue fname       = (mkFlagName (lowercase fname), True)

    noMinusF :: String -> String
    noMinusF ('-':'f':_) = error "don't use '-f' in flag assignments; just use the flag's name"
    noMinusF x           = x
