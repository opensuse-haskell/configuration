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
  let compiler = "ghc-8.6.2"
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
constraintList = [ "adjunctions"
                 , "aeson"
                 , "aeson-compat"
                 , "aeson-pretty"
                 , "alex"
                 , "annotated-wl-pprint"
                 , "ansi-terminal"
                 , "ansi-wl-pprint"
                 , "asn1-encoding"
                 , "asn1-parse"
                 , "asn1-types"
                 , "async"
                 , "attoparsec"
                 , "attoparsec-iso8601"
                 , "auto-update"
                 , "base-compat"
                 , "base-orphans"
                 , "base-prelude"
                 , "base16-bytestring"
                 , "base64-bytestring"
                 , "basement"
                 , "bifunctors"
                 , "bindings-uname"
                 , "bitarray"
                 , "blaze-builder"
                 , "blaze-html"
                 , "blaze-markup"
                 , "byteable"
                 , "cabal-doctest"
                 , "cabal-install == 2.4.0.0"
                 , "cabal2spec"
                 , "call-stack"
                 , "case-insensitive"
                 , "cereal"
                 , "clock"
                 , "cmark-gfm"
                 , "cmdargs"
                 , "colour"
                 , "comonad"
                 , "conduit"
                 , "conduit-extra"
                 , "connection"
                 , "constraints"
                 , "contravariant"
                 , "cookie"
                 , "cryptohash"
                 , "cryptohash-sha256"
                 , "cryptonite"
                 , "cryptonite-conduit"
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
                 , "doctemplates"
                 , "easy-file"
                 , "echo"
                 , "ed25519"
                 , "edit-distance"
                 , "either"
                 , "enclosed-exceptions"
                 , "erf"
                 , "exceptions"
                 , "extensible-exceptions"
                 , "extra"
                 , "fail"
                 , "fast-logger"
                 , "file-embed"
                 , "filelock"
                 , "foldl"
                 , "foundation"
                 , "free"
                 , "fsnotify"
                 , "generic-deriving"
                 , "gitrev"
                 , "Glob"
                 , "hackage-security"
                 , "haddock-library"
                 , "happy"
                 , "hashable"
                 , "haskell-src-exts"
                 , "hinotify"
                 , "hostname"
                 , "hourglass"
                 , "hpack"
                 , "hsemail"
                 , "hslua"
                 , "hslua-module-text"
                 , "hspec"
                 , "hspec-core"
                 , "hspec-discover"
                 , "hspec-expectations"
                 , "hspec-smallcheck"
                 , "HsYAML"
                 , "HTTP"
                 , "http-api-data"
                 , "http-client"
                 , "http-client-tls"
                 , "http-conduit"
                 , "http-types"
                 , "HUnit"
                 , "hxt"
                 , "hxt-charproperties"
                 , "hxt-regex-xmlschema"
                 , "hxt-unicode"
                 , "infer-license"
                 , "integer-logarithms"
                 , "invariant"
                 , "iwlib"
                 , "json"
                 , "JuicyPixels"
                 , "kan-extensions"
                 , "lens"
                 , "libxml-sax"
                 , "libyaml"
                 , "lifted-async"
                 , "lifted-base"
                 , "logict"
                 , "managed"
                 , "math-functions"
                 , "megaparsec"
                 , "memory"
                 , "microlens"
                 , "microlens-th"
                 , "mime-types"
                 , "mintty"
                 , "monad-control"
                 , "monad-logger"
                 , "monad-loops"
                 , "mono-traversable"
                 , "mustache"
                 , "mwc-random"
                 , "neat-interpolation"
                 , "network"
                 , "network-uri"
                 , "old-locale"
                 , "old-time"
                 , "open-browser"
                 , "optional-args"
                 , "optparse-applicative"
                 , "pandoc"
                 , "pandoc-types <1.19 || >1.19"
                 , "parallel"
                 , "parsec-class"
                 , "parsec-numbers"
                 , "parser-combinators"
                 , "path"
                 , "path-io"
                 , "path-pieces"
                 , "pem"
                 , "persistent"
                 , "persistent-sqlite"
                 , "persistent-template"
                 , "primitive"
                 , "profunctors"
                 , "project-template"
                 , "QuickCheck"
                 , "quickcheck-io"
                 , "random"
                 , "reflection"
                 , "regex-applicative"
                 , "regex-applicative-text"
                 , "regex-base"
                 , "regex-compat"
                 , "regex-pcre-builtin"
                 , "regex-posix"
                 , "regex-tdfa"
                 , "resolv"
                 , "resource-pool"
                 , "resourcet"
                 , "retry"
                 , "rio"
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
                 , "shelly"
                 , "silently"
                 , "skylighting"
                 , "skylighting-core"
                 , "smallcheck"
                 , "socks"
                 , "split"
                 , "stack < 9.9.9"
                 , "StateVar"
                 , "stm-chans"
                 , "store"
                 , "store-core"
                 , "streaming-commons"
                 , "syb"
                 , "system-fileio"
                 , "system-filepath"
                 , "tagged"
                 , "tagsoup"
                 , "tar"
                 , "temporary"
                 , "texmath"
                 , "text-metrics"
                 , "tf-random"
                 , "th-abstraction"
                 , "th-expand-syns"
                 , "th-lift"
                 , "th-lift-instances"
                 , "th-orphans"
                 , "th-reify-many"
                 , "th-utilities"
                 , "time-locale-compat"
                 , "tls"
                 , "transformers-base"
                 , "transformers-compat"
                 , "turtle"
                 , "typed-process"
                 , "unicode-transforms"
                 , "unix-compat"
                 , "unix-time"
                 , "unliftio"
                 , "unliftio-core"
                 , "unordered-containers"
                 , "uri-bytestring"
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
