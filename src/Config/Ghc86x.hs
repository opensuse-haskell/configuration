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
constraintList = [ "aeson"
                 , "aeson-compat"
                 , "alex"
                 , "ansi-terminal"
                 , "ansi-wl-pprint"
                 , "async"
                 , "attoparsec"
                 , "base-compat"
                 , "base-orphans"
                 , "base-prelude"
                 , "base16-bytestring"
                 , "base64-bytestring"
                 , "bifunctors"
                 , "blaze-markup"
                 , "cabal-doctest"
                 , "cabal-install"
                 , "cabal2spec"
                 , "clock"
                 , "colour"
                 , "comonad"
                 , "contravariant"
                 , "cryptohash-sha256"
                 , "data-default"
                 , "data-default-class"
                 , "data-default-instances-containers"
                 , "data-default-instances-dlist"
                 , "data-default-instances-old-locale"
                 , "Diff"
                 , "digest"
                 , "distribution-opensuse"
                 , "distributive"
                 , "dlist"
                 , "echo"
                 , "ed25519"
                 , "edit-distance"
                 , "exceptions"
                 , "extensible-exceptions"
                 , "extra"
                 , "foldl"
                 , "Glob"
                 , "hackage-security"
                 , "haddock-library"
                 , "happy"
                 , "hashable"
                 , "hostname"
                 , "hsc2hs"
                 , "hsemail"
                 , "hslua"
                 , "hslua-module-text"
                 , "HTTP"
                 , "integer-logarithms"
                 , "JuicyPixels"
                 , "lifted-async"
                 , "managed"
                 , "math-functions"
                 , "memory"
                 , "microlens-th"
                 , "mwc-random"
                 , "network < 2.8"                    -- https://github.com/haskell/HTTP/issues/120
                 , "network-uri"
                 , "old-locale"
                 , "old-time"
                 , "optional-args"
                 , "optparse-applicative"
                 , "parsec-class"
                 , "polyparse"
                 , "primitive"
                 , "profunctors"
                 , "random"
                 , "resolv"
                 , "scientific"
                 , "SDL"
                 , "SDL-image"
                 , "SDL-mixer"
                 , "semigroupoids"
                 , "semigroups"
                 , "setlocale"
                 , "ShellCheck"
                 , "StateVar"
                 , "system-fileio"
                 , "system-filepath"
                 , "tagged"
                 , "tar"
                 , "temporary"
                 , "texmath"
                 , "th-abstraction"
                 , "time-locale-compat"
                 , "transformers-compat"
                 , "turtle"
                 , "unix-compat"
                 , "unliftio"
                 , "unordered-containers"
                 , "uri-bytestring"
                 , "utf8-string"
                 , "uuid-types"
                 , "vector"
                 , "vector-builder"
                 , "vector-th-unbox"
                 , "X11"
                 , "X11-xft"
                 , "xmobar"
                 , "xmonad"
                 , "xmonad-contrib"
                 , "zip-archive"
                 , "zlib"
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
