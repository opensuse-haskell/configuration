{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

module Config where

import Config.ForcedExecutables
import Orphans ()
import Types

import Data.Map.Strict ( Map, findWithDefault, fromList )
import Data.Maybe
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Simple.Utils ( lowercase )
import Distribution.Text

packageSets :: Map PackageSetId PackageSetConfig
packageSets = [ ("ghc-8.4.x", ghc84x)
              ]

getPackageSet :: PackageSetId -> PackageSetConfig
getPackageSet psid = findWithDefault err psid packageSets
  where
    err = error ("unknown package set " ++ show (unPackageSetId psid))

ghc84x :: PackageSetConfig
ghc84x = PackageSetConfig
  { compiler         = "ghc-8.4.3"
  , targetPackages   = [ "alex >=3.2.4"
                       , "cabal-install >=2.2.0.0"
                       , "happy >=1.19.9"
                       , "pandoc >=2.2.1"
                       , "ShellCheck >=0.4.7"
                       , "stack >=1.7.1"

                         -- Dmitriy Perlow <dap.darkness@gmail.com> needs these
                         -- packages for games/raincat.
                       , "SDL >=0.6.6.0"
                       , "SDL-image >=0.6.1.2"
                       , "SDL-mixer >=0.6.2.0"
                       ]
  , packageSet       = [ "aeson-1.2.4.0"
                       , "aeson-compat-0.3.7.1"
                       , "aeson-pretty-0.8.7"
                       , "alex-3.2.4"
                       , "annotated-wl-pprint-0.7.0"
                       , "ansi-terminal-0.8.0.4"
                       , "ansi-wl-pprint-0.6.8.2"
                       , "asn1-encoding-0.9.5"
                       , "asn1-parse-0.9.4"
                       , "asn1-types-0.3.2"
                       , "async-2.2.1"
                       , "attoparsec-0.13.2.2"
                       , "attoparsec-iso8601-1.0.0.0"
                       , "auto-update-0.1.4"
                       , "base-compat-0.9.3"
                       , "base-orphans-0.7"
                       , "base-prelude-1.2.1"
                       , "base16-bytestring-0.1.1.6"
                       , "base64-bytestring-1.0.0.1"
                       , "basement-0.0.7"
                       , "bifunctors-5.5.2"
                       , "bindings-uname-0.1"
                       , "bitarray-0.0.1.1"
                       , "blaze-builder-0.4.1.0"
                       , "blaze-html-0.9.0.1"
                       , "blaze-markup-0.8.2.1"
                       , "byteable-0.1.1"
                       , "cabal-doctest-1.0.6"
                       , "cabal-install-2.2.0.0"
                       , "call-stack-0.1.0"
                       , "case-insensitive-1.2.0.11"
                       , "cereal-0.5.5.0"
                       , "clock-0.7.2"
                       , "cmark-gfm-0.1.3"
                       , "cmdargs-0.10.20"
                       , "colour-2.3.4"
                       , "comonad-5.0.3"
                       , "conduit-1.3.0.2"
                       , "conduit-extra-1.3.0"
                       , "connection-0.2.8"
                       , "contravariant-1.4.1"
                       , "cookie-0.4.4"
                       , "cpphs-1.20.8"
                       , "cryptohash-0.11.9"
                       , "cryptohash-sha256-0.11.101.0"
                       , "cryptonite-0.25"
                       , "cryptonite-conduit-0.2.2"
                       , "data-default-0.7.1.1"
                       , "data-default-class-0.1.2.0"
                       , "data-default-instances-containers-0.0.1"
                       , "data-default-instances-dlist-0.0.1"
                       , "data-default-instances-old-locale-0.0.1"
                       , "digest-0.0.1.2"
                       , "distributive-0.5.3"
                       , "dlist-0.8.0.4"
                       , "doctemplates-0.2.2.1"
                       , "easy-file-0.2.2"
                       , "echo-0.1.3"
                       , "ed25519-0.0.5.0"
                       , "edit-distance-0.2.2.1"
                       , "either-5"
                       , "exceptions-0.10.0"
                       , "extra-1.6.8"
                       , "fail-4.9.0.0"
                       , "fast-logger-2.4.11"
                       , "file-embed-0.0.10.1"
                       , "filelock-0.1.1.2"
                       , "foundation-0.0.20"
                       , "free-5.0.2"
                       , "fsnotify-0.2.1.2"
                       , "generic-deriving-1.12.1"
                       , "gitrev-1.3.1"
                       , "Glob-0.9.2"
                       , "hackage-security-0.5.3.0"
                       , "haddock-library-1.5.0.1"
                       , "happy-1.19.9"
                       , "hashable-1.2.7.0"
                       , "haskell-src-exts-1.20.2"
                       , "haskell-src-meta-0.8.0.2"
                       , "hinotify-0.3.10"
                       , "hourglass-0.2.11"
                       , "hpack-0.28.2"
                       , "hslua-0.9.5.2"
                       , "hslua-module-text-0.1.2.1"
                       , "hspec-2.5.1"
                       , "hspec-core-2.5.1"
                       , "hspec-discover-2.5.1"
                       , "hspec-expectations-0.8.2"
                       , "hspec-smallcheck-0.5.2"
                       , "HTTP-4000.3.11"
                       , "http-api-data-0.3.8.1"
                       , "http-client-0.5.12.1"
                       , "http-client-tls-0.3.5.3"
                       , "http-conduit-2.3.1"
                       , "http-types-0.12.1"
                       , "HUnit-1.6.0.0"
                       , "hxt-9.3.1.16"
                       , "hxt-charproperties-9.2.0.1"
                       , "hxt-regex-xmlschema-9.2.0.3"
                       , "hxt-unicode-9.0.2.4"
                       , "integer-logarithms-1.0.2.1"
                       , "JuicyPixels-3.2.9.5"
                       , "lifted-base-0.2.3.12"
                       , "logict-0.6.0.2"
                       , "memory-0.14.16"
                       , "microlens-0.4.9.1"
                       , "microlens-th-0.4.2.1"
                       , "mime-types-0.1.0.7"
                       , "mintty-0.1.2"
                       , "monad-control-1.0.2.3"
                       , "monad-logger-0.3.28.5"
                       , "monad-loops-0.4.3"
                       , "mono-traversable-1.0.8.1"
                       , "mustache-2.3.0"
                       , "neat-interpolation-0.3.2.1"
                       , "network-2.6.3.5"
                       , "network-uri-2.6.1.0"
                       , "old-locale-1.0.0.7"
                       , "old-time-1.1.0.3"
                       , "open-browser-0.2.1.0"
                       , "optparse-applicative-0.14.2.0"
                       , "optparse-simple-0.1.0"
                       , "pandoc-2.2.1"
                       , "pandoc-types-1.17.4.2"
                       , "path-0.6.1"
                       , "path-io-1.3.3"
                       , "path-pieces-0.2.1"
                       , "pem-0.2.4"
                       , "persistent-2.8.2"
                       , "persistent-sqlite-2.8.1.2"
                       , "persistent-template-2.5.4"
                       , "polyparse-1.12"
                       , "primitive-0.6.4.0"
                       , "profunctors-5.2.2"
                       , "project-template-0.2.0.1"
                       , "QuickCheck-2.11.3"
                       , "quickcheck-io-0.2.0"
                       , "random-1.1"
                       , "regex-applicative-0.3.3"
                       , "regex-applicative-text-0.1.0.1"
                       , "regex-base-0.93.2"
                       , "regex-pcre-builtin-0.94.4.8.8.35"
                       , "regex-tdfa-1.2.3"
                       , "resolv-0.1.1.1"
                       , "resource-pool-0.2.3.2"
                       , "resourcet-1.2.1"
                       , "retry-0.7.6.2"
                       , "rio-0.1.2.0"
                       , "safe-0.3.17"
                       , "scientific-0.3.6.2"
                       , "SDL-0.6.6.0"
                       , "SDL-image-0.6.1.2"
                       , "SDL-mixer-0.6.2.0"
                       , "semigroupoids-5.2.2"
                       , "semigroups-0.18.4"
                       , "setenv-0.1.1.3"
                       , "SHA-1.6.4.4"
                       , "ShellCheck-0.5.0"
                       , "silently-1.2.5"
                       , "skylighting-0.7.1"
                       , "skylighting-core-0.7.1"
                       , "smallcheck-1.1.4"
                       , "socks-0.5.6"
                       , "split-0.2.3.3"
                       , "stack-1.7.1"
                       , "StateVar-1.1.1.0"
                       , "stm-chans-3.0.0.4"
                       , "store-0.5.0"
                       , "store-core-0.4.3"
                       , "streaming-commons-0.2.0.0"
                       , "syb-0.7"
                       , "tagged-0.8.5"
                       , "tagsoup-0.14.6"
                       , "tar-0.5.1.0"
                       , "temporary-1.2.1.1"
                       , "texmath-0.11.0.1"
                       , "text-metrics-0.3.0"
                       , "tf-random-0.5"
                       , "th-abstraction-0.2.6.0"
                       , "th-expand-syns-0.4.4.0"
                       , "th-lift-0.7.10"
                       , "th-lift-instances-0.1.11"
                       , "th-orphans-0.13.5"
                       , "th-reify-many-0.1.8"
                       , "th-utilities-0.2.0.1"
                       , "time-locale-compat-0.1.1.4"
                       , "tls-1.4.1"
                       , "transformers-base-0.4.5.2"
                       , "transformers-compat-0.6.2"
                       , "typed-process-0.2.2.0"
                       , "unicode-transforms-0.3.4"
                       , "unix-compat-0.5.0.1"
                       , "unix-time-0.3.8"
                       , "unliftio-0.2.7.0"
                       , "unliftio-core-0.1.1.0"
                       , "unordered-containers-0.2.9.0"
                       , "uri-bytestring-0.3.2.0"
                       , "utf8-string-1.0.1.1"
                       , "uuid-types-1.0.3"
                       , "vector-0.12.0.1"
                       , "vector-algorithms-0.7.0.1"
                       , "void-0.7.2"
                       , "x509-1.7.3"
                       , "x509-store-1.6.6"
                       , "x509-system-1.6.6"
                       , "x509-validation-1.6.10"
                       , "xml-1.3.14"
                       , "yaml-0.8.30"
                       , "zip-archive-0.3.2.5"
                       , "zlib-0.6.2"
                       ]
  , flagAssignments  = fromList (readFlagAssignents flagList)
  , forcedExectables = forcedExectableNames
  }

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
