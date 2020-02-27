{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Config.Ghc88x ( ghc88x, resolveConstraints ) where

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

ghc88x :: Action PackageSetConfig
ghc88x = do
  let compiler = "ghc-8.8.1"
      flagAssignments = fromList (readFlagAssignents flagList)
      forcedExectables = forcedExectableNames
  packageSet <- fromList <$>
                  forM (toList constraintList) (\(pn,vr) ->
                    (,) pn <$> askOracle (PackageVersionConstraint pn vr))
  pure (PackageSetConfig {..})

targetPackages :: ConstraintSet
targetPackages   = [ "alex >=3.2.4"
                   , "cabal-install >=2.2.0.0"
                   , "cabal2spec >2.2"
                   , "distribution-opensuse >1"
                   , "happy >=1.19.9"
                   , "pandoc >=2.2.1"
                   , "pandoc-citeproc"
                   , "ShellCheck >=0.4.7"
                   , "stack >=1.7.1 && < 9.9.9"
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
                 , "bitarray"
                 , "blaze-builder"
                 , "blaze-html"
                 , "blaze-markup"
                 , "byteable"
                 , "cabal-doctest"
                 , "cabal-install"
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
                 , "conduit-combinators"
                 , "conduit-extra"
                 , "connection"
                 , "constraints"
                 , "contravariant"
                 , "cookie"
                 , "cryptohash"
                 , "cryptohash-conduit"
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
                 , "doclayout < 0.3"    -- constraint via pandoc
                 , "doctemplates < 0.8.1"
                 , "easy-file"
                 , "echo"
                 , "ed25519"
                 , "edit-distance"
                 , "either"
                 , "emojis"
                 , "enclosed-exceptions"
                 , "errors"
                 , "exceptions"
                 , "extensible-exceptions"
                 , "extra"
                 , "fail"
                 , "fast-logger"
                 , "file-embed"
                 , "filelock"
                 , "foldl"
                 , "free"
                 , "fsnotify"
                 , "generic-deriving"
                 , "githash"
                 , "gitrev"
                 , "Glob"
                 , "hackage-security <0.6.0.0"
                 , "haddock-library"
                 , "happy"
                 , "hashable"
                 , "hi-file-parser"
                 , "hinotify"
                 , "hostname"
                 , "hourglass"
                 , "hpack"
                 , "hs-bibutils"
                 , "hsc2hs"
                 , "hsemail"
                 , "hslua"
                 , "hslua-module-system"
                 , "hslua-module-text"
                 , "HsYAML"
                 , "HsYAML-aeson"
                 , "HTTP"
                 , "http-api-data"
                 , "http-client"
                 , "http-client-tls"
                 , "http-conduit"
                 , "http-download"
                 , "http-types"
                 , "hxt"
                 , "hxt-charproperties"
                 , "hxt-regex-xmlschema"
                 , "hxt-unicode"
                 , "infer-license"
                 , "integer-logarithms"
                 , "invariant"
                 , "ipynb"
                 , "iwlib"
                 , "jira-wiki-markup"
                 , "JuicyPixels"
                 , "kan-extensions"
                 , "lens"
                 , "libyaml"
                 , "lifted-async"
                 , "lifted-base"
                 , "lukko"
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
                 , "optparse-simple"
                 , "pandoc"
                 , "pandoc-citeproc"
                 , "pandoc-types"
              -- , "pantry"     -- build is currently broken with ghc 8.8.x
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
                 , "rfc5051"
                 , "rio"
                 , "rio-orphans"
                 , "rio-prettyprint"
                 , "safe"
                 , "safe-exceptions"
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
                 , "socks"
                 , "split"
                 , "splitmix"
              -- , "stack < 9.9.9"      -- broken because of pantry: https://github.com/commercialhaskell/stack/issues/5132
                 , "StateVar"
                 , "stm-chans"
                 , "streaming-commons"
                 , "syb"
                 , "system-fileio"
                 , "system-filepath"
                 , "tagged"
                 , "tagsoup"
                 , "tar"
                 , "tar-conduit"
                 , "temporary"
                 , "terminal-size"
                 , "texmath"
                 , "text-conversions"
                 , "text-metrics"
                 , "th-abstraction"
                 , "th-expand-syns"
                 , "th-lift"
                 , "th-lift-instances"
                 , "th-orphans"
                 , "th-reify-many"
                 , "th-utilities"
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
                 , "unix-time"
                 , "unliftio"
                 , "unliftio-core"
                 , "unordered-containers"
                 , "utf8-string"
                 , "uuid-types"
                 , "vector"
                 , "vector-algorithms"
                 , "vector-binary-instances"
                 , "vector-builder"
                 , "vector-th-unbox"
                 , "void"
                 , "word8"
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
               , "base ==4.13.0.0"
               , "binary ==0.8.7.0"
               , "bytestring ==0.10.9.0"
               , "Cabal ==3.0.0.0"
               , "containers ==0.6.2.1"
               , "deepseq ==1.4.4.0"
               , "directory ==1.3.3.2"
               , "filepath ==1.4.2.1"
               , "ghc ==8.8.1"
               , "ghc-boot ==8.8.1"
               , "ghc-boot-th ==8.8.1"
               , "ghc-compact ==0.1.0.0"
               , "ghc-heap ==8.8.1"
               , "ghc-prim ==0.5.3"
               , "ghci ==8.8.1"
               , "haskeline ==0.7.5.0"
               , "hpc ==0.6.0.3"
               , "integer-gmp ==1.0.2.0"
               , "libiserv ==8.8.1"
               , "mtl ==2.2.2"
               , "parsec ==3.1.14.0"
               , "pretty ==1.1.3.6"
               , "process ==1.6.5.1"
               , "rts ==1.0"
               , "stm ==2.5.0.0"
               , "template-haskell ==2.15.0.0"
               , "terminfo ==0.4.1.4"
               , "text ==1.2.4.0"
               , "time ==1.9.3"
               , "transformers ==0.5.6.2"
               , "unix ==2.7.2.2"
               , "xhtml ==3000.2.2.1"
               ]
