{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Config.Ghc810x ( ghc810x, resolveConstraints ) where

import Config.ForcedExecutables
import Oracle.Hackage ( )
import Types

import Control.Monad
import Data.List ( intercalate )
import Data.Map.Strict ( fromList, toList, keys, union )
import Data.Maybe
import Development.Shake
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Simple.Utils ( lowercase )
import Distribution.Text
import Distribution.Types.PackageVersionConstraint

ghc810x :: Action PackageSetConfig
ghc810x = do
  let compiler = "ghc-8.10.1"
      flagAssignments = fromList (readFlagAssignents flagList)
      forcedExectables = forcedExectableNames
  packageSet <- fromList <$>
                  forM (toList constraintList) (\(pn,vr) ->
                    (,) pn <$> askOracle (PackageVersionConstraint pn vr))
  pure (PackageSetConfig {..})

targetPackages :: ConstraintSet
targetPackages   = [ "alex >=3.2.4"
                   , "cabal-install ==3.2.*"
                   , "cabal2spec >2.2"
                   , "distribution-opensuse >1"
                   , "happy >=1.19.9"
                   , "pandoc >=2.2.1"
                   , "pandoc-citeproc"
                   , "ShellCheck >=0.4.7"
                -- , "stack >=1.7.1 && < 9.9.9"
                   , "xmobar >= 0.27"
                   , "xmonad >= 0.14"
                   , "xmonad-contrib >= 0.14"
                   , "SDL >=0.6.6.0"        -- Dmitriy Perlow <dap.darkness@gmail.com> needs the
                   , "SDL-image >=0.6.1.2"  -- SDL packages for games/raincat.
                   , "SDL-mixer >=0.6.2.0"
                   ]

resolveConstraints :: String
resolveConstraints = unwords ["cabal", "new-install", "--dry-run", "--minimize-conflict-set", constraints, flags, pkgs]
  where
    pkgs = intercalate " " (display <$> keys targetPackages)
    constraints = "--constraint=" <> intercalate " --constraint=" (show <$> environment)
    environment = display . (\(n,v) -> PackageVersionConstraint n v) <$> toList (corePackages `union` targetPackages)
    flags = unwords [ "--constraint=" <> show (unwords [unPackageName pn, flags'])
                    | pn <- keys targetPackages
                    , Just flags' <- [lookup (unPackageName pn) flagList]
                    ]

constraintList :: ConstraintSet
constraintList = [ "adjunctions"
                 , "aeson"
                 , "aeson-pretty"
                 , "alex"
                 , "ansi-terminal"
                 , "ansi-wl-pprint"
                 , "asn1-encoding"
                 , "asn1-parse"
                 , "asn1-types"
                 , "async"
                 , "attoparsec"
                 , "base-compat"
                 , "base-orphans"
                 , "base-prelude"
                 , "base16-bytestring"
                 , "base64-bytestring"
                 , "basement"
                 , "bifunctors"
                 , "bitarray"
                 , "blaze-builder"
                 , "blaze-html"
                 , "blaze-markup"
                 , "cabal-doctest"
                 , "cabal-install ==3.2.*"
                 , "cabal2spec"
                 , "call-stack"
                 , "case-insensitive"
                 , "cereal"
                 , "clock"
                 , "cmark-gfm"
                 , "colour"
                 , "comonad"
                 , "conduit"
                 , "conduit-extra"
                 , "connection"
                 , "contravariant"
                 , "cookie"
                 , "cryptohash-sha256"
                 , "cryptonite"
                 , "data-default"
                 , "data-default-class"
                 , "data-default-instances-containers"
                 , "data-default-instances-dlist"
                 , "data-default-instances-old-locale"
                 , "dbus"
                 , "Diff"
                 , "digest"
                 , "distribution-opensuse"
                 , "distributive"
                 , "dlist"
                 , "doclayout"
                 , "doctemplates"
                 , "echo"
                 , "ed25519"
                 , "edit-distance"
                 , "emojis"
                 , "errors"
                 , "extensible-exceptions"
                 , "extra"
                 , "fail"
                 , "foldl"
                 , "free"
                 , "Glob"
                 , "hackage-security"
                 , "haddock-library"
                 , "happy"
                 , "hashable"
                 , "hinotify"
                 , "hostname"
                 , "hourglass"
                 , "hs-bibutils"
                 , "hsemail"
                 , "hslua"
                 , "hslua-module-system"
                 , "hslua-module-text"
                 , "HsYAML"
                 , "HsYAML-aeson"
                 , "HTTP"
                 , "http-client"
                 , "http-client-tls"
                 , "http-conduit"
                 , "http-types"
                 , "hxt"
                 , "hxt-charproperties"
                 , "hxt-regex-xmlschema"
                 , "hxt-unicode"
                 , "integer-logarithms"
                 , "invariant"
                 , "ipynb"
                 , "iwlib"
                 , "jira-wiki-markup"
                 , "JuicyPixels"
                 , "kan-extensions"
                 , "lens"
                 , "libyaml"
                 , "lukko"
                 , "managed"
                 , "math-functions"
                 , "memory"
                 , "mime-types"
                 , "mono-traversable"
                 , "mwc-random"
                 , "network"
                 , "network-uri < 2.7"
                 , "old-locale"
                 , "old-time"
                 , "optional-args"
                 , "optparse-applicative"
                 , "pandoc"
                 , "pandoc-citeproc"
                 , "pandoc-types"
                 , "parallel"
                 , "parsec-class"
                 , "parsec-numbers"
                 , "pem"
                 , "primitive"
                 , "profunctors"
                 , "QuickCheck"
                 , "random"
                 , "reflection"
                 , "regex-base"
                 , "regex-compat"
                 , "regex-pcre-builtin"
                 , "regex-posix"
                 , "regex-tdfa"
                 , "resolv"
                 , "resourcet"
                 , "rfc5051"
                 , "safe"
                 , "scientific"
                 , "SDL"
                 , "SDL-image"
                 , "SDL-mixer"
                 , "semigroupoids"
                 , "semigroups"
                 , "setenv"
                 , "setlocale"
                 , "SHA"
                 , "ShellCheck"
                 , "skylighting"
                 , "skylighting-core"
                 , "socks"
                 , "split"
                 , "splitmix"
                 , "StateVar"
                 , "streaming-commons"
                 , "syb"
                 , "system-fileio"
                 , "system-filepath"
                 , "tagged"
                 , "tagsoup"
                 , "tar"
                 , "temporary"
                 , "texmath"
                 , "text-conversions"
                 , "th-abstraction"
                 , "th-lift"
                 , "time-compat"
                 , "timezone-olson"
                 , "timezone-series"
                 , "tls"
                 , "transformers-base"
                 , "transformers-compat"
                 , "turtle"
                 , "type-equality"
                 , "typed-process"
                 , "unicode-transforms"
                 , "unix-compat"
                 , "unliftio-core"
                 , "unordered-containers"
                 , "utf8-string"
                 , "uuid-types"
                 , "vector"
                 , "vector-algorithms"
                 , "vector-builder"
                 , "vector-th-unbox"
                 , "void"
                 , "X11"
                 , "X11-xft"
                 , "x509"
                 , "x509-store"
                 , "x509-system"
                 , "x509-validation"
                 , "xml"
                 , "xml-conduit"
                 , "xml-types"
                 , "xmobar"
                 , "xmonad"
                 , "xmonad-contrib"
                 , "yaml"
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
  , ("xmobar",                         "with_thread with_utf8 with_xft with_xpm with_mpris with_dbus with_iwlib with_inotify with_datezone")

    -- Enable additional features
  , ("idris",                          "ffi gmp")

    -- Disable dependencies we don't have.
  , ("invertible",                     "-hlist -piso")

    -- Use the system sqlite library rather than the bundled one.
  , ("persistent-sqlite",              "systemlib")

    -- Since version 6.20170925, the test suite can no longer be run outside of
    -- a checked-out copy of the git repository.
  , ("git-annex",                      "-testsuite")

    -- Compile against the system library, not the one bundled in the Haskell
    -- source tarball.
  , ("cmark",                          "pkgconfig")

    -- Fix build with modern compilers.
  , ("cassava",                        "-bytestring--lt-0_10_4")

    -- Prefer the system's library over the bundled one.
  , ("libyaml",                        "system-libyaml")

    -- Configure a production-like build environment.
  , ("stack",                          "hide-dependency-versions disable-git-info supported-build")
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

corePackages :: ConstraintSet
corePackages = [ "array ==0.5.4.0"
               , "base ==4.14.0.0"
               , "binary ==0.8.7.0"
               , "bytestring ==0.10.9.0"
               , "Cabal ==3.1.0.0"
               , "containers ==0.6.2.1"
               , "deepseq ==1.4.4.0"
               , "directory ==1.3.4.0"
               , "exceptions ==0.10.3"
               , "filepath ==1.4.2.1"
               , "ghc ==8.10.0.20191210"
               , "ghc-boot ==8.10.0.20191210"
               , "ghc-boot-th ==8.10.0.20191210"
               , "ghc-compact ==0.1.0.0"
               , "ghc-heap ==8.10.0.20191210"
               , "ghc-prim ==0.6.1"
               , "ghci ==8.10.0.20191210"
               , "haskeline ==0.8.0.0"
               , "hpc ==0.6.0.3"
               , "hsc2hs ==0.68.6"
               , "integer-gmp ==1.0.2.0"
               , "libiserv ==8.10.0.20191210"
               , "mtl ==2.2.2"
               , "parsec ==3.1.14.0"
               , "pretty ==1.1.3.6"
               , "process ==1.6.6.0"
               , "rts ==1.0"
               , "stm ==2.5.0.0"
               , "template-haskell ==2.16.0.0"
               , "terminfo ==0.4.1.4"
               , "text ==1.2.3.1"
               , "time ==1.9.3"
               , "transformers ==0.5.6.2"
               , "unix ==2.7.2.2"
               , "xhtml ==3000.2.2.1"
               ]
