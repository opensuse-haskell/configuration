{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Config.Ghc810x ( ghc810x ) where

import Config.ForcedExecutables
import Oracle.Hackage ( )
import Types
import MyCabal

import Control.Monad
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map.Strict ( fromList, toList )
import Data.Maybe
import qualified Data.Set as Set
import Development.Shake

ghc810x :: Action PackageSetConfig
ghc810x = do
  let compiler = "ghc-8.10.7"
      flagAssignments = fromList (readFlagAssignents flagList)
      forcedExectables = forcedExectableNames
      corePackages = ghcCorePackages
  packageSet <- fromList <$>
                  forM (toList constraintList) (\(pn,vr) ->
                    (,) pn <$> askOracle (PackageVersionConstraint pn vr))
  checkConsistency (PackageSetConfig {..})

{-
targetPackages :: ConstraintSet
targetPackages   = [ "alex >=3.2.5"
                   , "cabal-install ==3.2.*"
                   , "cabal2spec >=2.6"
                   , "cabal-plan"
                   , "distribution-opensuse >= 1.1.1"
                   , "git-annex"
                   , "happy >=1.19.12"
                   , "hledger", "hledger-ui", "hledger-interest"
                   , "hlint"
                   , "ghcid"
                   , "pandoc >=2.9.2.1"
                   , "citeproc >=0.17"
                   , "postgresql-simple"    -- needed by emu-incident-report
                   , "SDL >=0.6.6.0"        -- Dmitriy Perlow <dap.darkness@gmail.com> needs the
                   , "SDL-image >=0.6.2.0"  -- SDL packages for games/raincat.
                   , "SDL-mixer >=0.6.3.0"
                   , "shake"
                   , "ShellCheck >=0.7.1"
                   , "weeder"
                   , "xmobar >= 0.33"
                   , "xmonad >= 0.15"
                   , "xmonad-contrib >= 0.16"
                   ]

resolveConstraints :: String
resolveConstraints = unwords ["cabal", "install", "--dry-run", "--minimize-conflict-set", constraints, flags, pkgs]
  where
    pkgs = intercalate " " (display <$> keys targetPackages)
    constraints = "--constraint=" <> intercalate " --constraint=" (show <$> environment)
    environment = display . (\(n,v) -> PackageVersionConstraint n v) <$> toList (corePackages `union` targetPackages)
    flags = unwords [ "--constraint=" <> show (unwords [unPackageName pn, flags'])
                    | pn <- keys targetPackages
                    , Just flags' <- [lookup (unPackageName pn) flagList]
                    ]
 -}

constraintList :: ConstraintSet
constraintList = [ "abstract-deque"
                 , "abstract-par"
                 , "adjunctions"
                 , "aeson < 2"
                 , "aeson-pretty"
                 , "aeson-yaml"
                 , "alex"
                 , "algebraic-graphs < 0.6"     -- weeder doesn't support newer versions
                 , "alsa-core"
                 , "alsa-mixer"
                 , "annotated-wl-pprint"
                 , "ansi-terminal"
                 , "ansi-wl-pprint"
                 , "ap-normalize"
                 , "appar"
                 , "ascii-progress"
                 , "asn1-encoding"
                 , "asn1-parse"
                 , "asn1-types"
                 , "assoc"
                 , "async"
                 , "async-timer"
                 , "atomic-primops"
                 , "atomic-write"
                 , "attoparsec"
                 , "attoparsec-iso8601"
                 , "authenticate-oauth"
                 , "auto-update"
                 , "aws < 0.22.1"               -- avoid need for aeson 2.x
                 , "barbies"
                 , "base-compat"
                 , "base-compat-batteries"
                 , "base-noprelude"
                 , "base-orphans"
                 , "base-prelude"
                 , "base16-bytestring"
                 , "base58-bytestring"
                 , "base64"
                 , "base64-bytestring"
                 , "base64-bytestring-type"
                 , "basement"
                 , "bech32"
                 , "bech32-th"
                 , "bencode"
                 , "bifunctors"
                 , "bimap"
                 , "binary-orphans"
                 , "bindings-DSL"
                 , "bindings-uname"
                 , "bitarray"
                 , "bitmap"
                 , "bitvec"
                 , "blaze-builder"
                 , "blaze-html"
                 , "blaze-markup"
                 , "bloomfilter"
                 , "boring"
                 , "boxes"
                 , "breakpoint"
                 , "brick"
                 , "bsb-http-chunked"
                 , "byteable"
                 , "byteorder"
                 , "bytestring-builder"
                 , "bzlib-conduit"
                 , "c2hs"
                 , "cabal-doctest"
                 , "cabal-install ==3.2.*"
                 , "cabal-plan < 0.7.2.2"       -- newer versions need aeson 2.x
                 , "Cabal-syntax <3.9"
                 , "cabal2spec < 2.6.3"         -- the latest version requires Cabal-3.6.x.x
                 , "call-stack"
                 , "canonical-json"
                 , "case-insensitive"
                 , "casing"
                 , "cassava"
                 , "cassava-megaparsec"
                 , "cborg"
                 , "cborg-json"
                 , "cereal"
                 , "charset"
                 , "cipher-aes"
                 , "citeproc < 0.8"     -- pandoc doesn't support the latest version yet
                 , "clay"
                 , "clientsession"
                 , "clock"
                 , "cmark-gfm"
                 , "cmdargs"
                 , "code-page"
                 , "colour"
                 , "commonmark"
                 , "commonmark-extensions"
                 , "commonmark-pandoc"
                 , "comonad"
                 , "concurrency"
                 , "concurrent-output"
                 , "conduit"
                 , "conduit-combinators"
                 , "conduit-extra"
                 , "conduit-zstd"
                 , "config-ini"
                 , "connection"
                 , "constraints"
                 , "contravariant"
                 , "control-monad-free"
                 , "cookie"
                 , "cpphs"
                 , "cprng-aes"
                 , "criterion"
                 , "criterion-measurement"
                 , "crypto-api"
                 , "crypto-cipher-types"
                 , "crypto-pubkey-types"
                 , "crypto-random"
                 , "cryptohash"
                 , "cryptohash-conduit"
                 , "cryptohash-md5"
                 , "cryptohash-sha1"
                 , "cryptohash-sha256"
                 , "cryptonite"
                 , "cryptonite-conduit"
                 , "css-text"
                 , "csv"
                 , "data-clist"
                 , "data-default"
                 , "data-default-class"
                 , "data-default-instances-containers"
                 , "data-default-instances-dlist"
                 , "data-default-instances-old-locale"
                 , "data-fix"
                 , "DAV"
                 , "dbus <1.2.23"       -- the new version requires template-haskell 2.18.x
                 , "dec"
                 , "Decimal"
                 , "dense-linear-algebra"
                 , "deriving-aeson >=0.2"
                 , "dhall < 1.41"       -- newer versions break weeder
                 , "dhall-json < 1.7.10"  -- newer versions require dhall 1.41
                 , "dhall-yaml"
                 , "Diff"
                 , "digest"
                 , "disk-free-space"
                 , "distribution-opensuse"
                 , "distributive"
                 , "dlist"
                 , "dns"
                 , "doclayout"
                 , "doctemplates"
                 , "dotgen"
                 , "double-conversion"
                 , "easy-file"
                 , "echo"
                 , "ed25519"
                 , "edit-distance"
                 , "either"
                 , "ekg"
                 , "ekg-core"
                 , "ekg-json"
                 , "email-validate"
                 , "emojis"
                 , "enclosed-exceptions"
                 , "entropy"
                 , "erf"
                 , "errors"
                 , "extensible-exceptions"
                 , "extra < 1.7.10" -- hlint is broken with new extra
                 , "fail"
                 , "fast-logger"
                 , "fdo-notify"
                 , "feed"
                 , "fgl"
                 , "file-embed"
                 , "filelock"
                 , "filemanip"
                 , "filepath-bytestring < 1.4.2.1.10"   -- the latest version requires a more modern bytestring library
                 , "filepattern"
                 , "filtrable"
                 , "fingertree"
                 , "fmlist"
                 , "fmt"
                 , "foldl"
                 , "formatting"
                 , "foundation"
                 , "free"
                 , "fsnotify < 0.4"     -- avoid this update for hledger-ui
                 , "generic-data"
                 , "generic-deriving"
                 , "generic-lens"
                 , "generic-lens-core"
                 , "generic-monoid"
                 , "generic-random"
                 , "ghc-byteorder"
                 , "ghc-lib-parser-ex <= 8.10.0.19" -- 8.10.0.20+ needs ghc9
                 , "ghc-paths"
                 , "ghcid"
                 , "git-annex"
                 , "git-lfs"
                 , "githash"
                 , "gitrev"
                 , "Glob"
                 , "gray-code"
                 , "gridtables"
                 , "groups"
                 , "hackage-security < 0.6.2.0" -- avoid pulling in Cabal-syntax
                 , "haddock-library < 1.11"
                 , "half"
                 , "happy <1.21.0 || >1.21.0"   -- 1.21.0 is deprecated on Hackage
                 , "hashable < 1.4.0.0"         -- TODO: try to remove this constraint
                 , "hashtables < 1.3"           -- version 1.3 and beyond need hashable 1.4.x
                 , "haskell-lexer"
                 , "haskell-src-exts"
                 , "haskell-src-meta"
                 , "heaps"
                 , "hedgehog < 1.1.3"           -- keep tasty-hedgehog building
                 , "hedgehog-corpus"
                 , "hedgehog-quickcheck"
                 , "hi-file-parser"
                 , "hinotify"
                 , "hjsmin"
                 , "hledger"
                 , "hledger-interest"
                 , "hledger-lib"
                 , "hledger-ui"
                 , "hlint <= 3.2.7" -- newer versions needs ghc-lib-parser-9.0.x
                 , "hostname"
                 , "hourglass"
                 , "hpack"
                 , "hs-bibutils"
                 , "hscolour"
                 , "hsemail"
                 , "hslogger"
                 , "hslua"
                 , "hslua-aeson"
                 , "hslua-classes"
                 , "hslua-core"
                 , "hslua-marshalling"
                 , "hslua-module-doclayout"
                 , "hslua-module-path"
                 , "hslua-module-system"
                 , "hslua-module-text"
                 , "hslua-module-version"
                 , "hslua-objectorientation"
                 , "hslua-packaging"
                 , "hspec"
                 , "hspec-core"
                 , "hspec-discover"
                 , "hspec-expectations"
                 , "hspec-golden-aeson"
                 , "hspec-smallcheck"
                 , "HsYAML"
                 , "HsYAML-aeson"
                 , "hsyslog"
                 , "html"
                 , "HTTP < 4000.4"              -- don't update to 4000.4 because it breaks cabal-install
                 , "http-api-data"
                 , "http-client"
                 , "http-client-restricted"
                 , "http-client-tls"
                 , "http-conduit"
                 , "http-date"
                 , "http-media"
                 , "http-types"
                 , "http2"
                 , "HUnit"
                 , "hxt"
                 , "hxt-charproperties"
                 , "hxt-regex-xmlschema"
                 , "hxt-unicode"
                 , "IfElse"
                 , "indexed-profunctors"
                 , "indexed-traversable"
                 , "indexed-traversable-instances"
                 , "infer-license"
                 , "integer-logarithms"
                 , "intern"
                 , "invariant"
                 , "io-streams"
                 , "io-streams-haproxy"
                 , "iproute"
                 , "ipynb"
                 , "iso8601-time"
                 , "iwlib"
                 , "jira-wiki-markup"
                 , "js-chart"
                 , "js-dgtable"
                 , "js-flot"
                 , "js-jquery"
                 , "JuicyPixels < 3.3.8"        -- newer versions require vector 0.13
                 , "kan-extensions"
                 , "katip"
                 , "language-c"
                 , "language-javascript"
                 , "lens"
                 , "lens-aeson < 1.2"           -- newer versions require aeson 2.x
                 , "lens-family-core"
                 , "libmpd"
                 , "libsystemd-journal"
                 , "libxml-sax"
                 , "libyaml"
                 , "lift-type"
                 , "lifted-async"
                 , "lifted-base"
                 , "ListLike"
                 , "logict"
                 , "lpeg >=1.0.1 && <1.1"
                 , "lua"
                 , "lucid"
                 , "lukko"
                 , "magic"
                 , "managed"
                 , "math-functions"
                 , "megaparsec < 9.2.2"
                 , "memory"
                 , "mersenne-random-pure64"
                 , "microlens"
                 , "microlens-ghc"
                 , "microlens-mtl"
                 , "microlens-platform"
                 , "microlens-th"
                 , "microstache"
                 , "mime-mail"
                 , "mime-types"
                 , "mintty"
                 , "mmorph"
                 , "monad-control"
                 , "monad-logger"
                 , "monad-loops"
                 , "monad-par"
                 , "monad-par-extras"
                 , "MonadRandom"
                 , "mono-traversable"
                 , "moo"
                 , "mountpoints"
                 , "mtl-compat"
                 , "mustache"
                 , "mwc-random"
                 , "neat-interpolation"
                 , "netlink"
                 , "network"
                 , "network-bsd"
                 , "network-byte-order"
                 , "network-info"
                 , "network-multicast"
                 , "network-uri < 2.7.0.0 || > 2.7.0.0"
                 , "nothunks"
                 , "OddWord"
                 , "old-locale"
                 , "old-time"
                 , "OneTuple"
                 , "Only"
                 , "open-browser"
                 , "optics"
                 , "optics-core"
                 , "optics-extra"
                 , "optics-th"
                 , "optional-args"
                 , "optparse-applicative"
                 , "optparse-generic"
                 , "optparse-generic"
                 , "optparse-simple"
                 , "pandoc"
                 , "pandoc-lua-marshal >=0.1.3.1 && <0.2"
                 , "pandoc-types"
                 , "parallel"
                 , "parsec-class"
                 , "parsec-numbers"
                 , "parser-combinators"
                 , "parsers"
                 , "partial-order"
                 , "path"
                 , "path-io"
                 , "path-pieces"
                 , "pem"
                 , "persistent < 2.12.0.0 || > 2.12.0.0" -- buggy release
                 , "persistent-sqlite"
                 , "persistent-template"
                 , "pipes"
                 , "pipes-safe"
                 , "polyparse"
                 , "postgresql-libpq"
                 , "postgresql-simple"
                 , "pretty-hex"
                 , "pretty-show"
                 , "pretty-simple < 4.1"        -- newer versions break our old version of dhall
                 , "prettyprinter"
                 , "prettyprinter-ansi-terminal"
                 , "primitive < 0.7.4.0 || > 0.7.4.0"   -- version 0.7.4.0 breaks cborg
                 , "process-extras"
                 , "profunctors"
                 , "prometheus"
                 , "protolude"
                 , "psqueues"
                 , "QuickCheck"
                 , "quickcheck-arbitrary-adt"
                 , "quickcheck-instances"
                 , "quickcheck-io"
                 , "quiet"
                 , "random"        -- don't update yet (2020-07-07)
                 , "random-shuffle"
                 , "readable"
                 , "recursion-schemes"
                 , "recv"
                 , "reducers"
                 , "refact"
                 , "reflection"
                 , "regex-applicative"
                 , "regex-applicative-text"
                 , "regex-base"
                 , "regex-compat"
                 , "regex-pcre-builtin"
                 , "regex-posix"
                 , "regex-tdfa"
                 , "repline"
                 , "resolv"
                 , "resource-pool"
                 , "resourcet"
                 , "retry"
                 , "rfc5051"
                 , "rio"
                 , "rio-orphans"
                 , "rio-prettyprint"
                 , "RSA"
                 , "safe"
                 , "safe-exceptions"
                 , "SafeSemaphore"
                 , "sandi"
                 , "sandwich"
                 , "scientific"
                 , "scrypt"
                 , "SDL"
                 , "SDL-image"
                 , "SDL-mixer"
                 , "securemem"
                 , "semialign"
                 , "semigroupoids"
                 , "semigroups"
                 , "serialise"
                 , "servant"
                 , "servant-client"
                 , "servant-client-core"
                 , "servant-server"
                 , "setenv"
                 , "setlocale"
                 , "SHA"
                 , "shake"
                 , "shakespeare"
                 , "ShellCheck"
                 , "shelltestrunner"
                 , "shelly"
                 , "show-combinators"
                 , "silently"
                 , "simple-sendfile"
                 , "singleton-bool"
                 , "skein"
                 , "skylighting"
                 , "skylighting-core"
                 , "skylighting-format-ansi"
                 , "skylighting-format-blaze-html"
                 , "skylighting-format-latex"
                 , "smallcheck"
                 , "smtp-mail"
                 , "snap-core"
                 , "snap-server"
                 , "socks"
                 , "some"
                 , "sop-core"
                 , "split"
                 , "splitmix"
                 , "StateVar"
                 , "statistics"
                 , "statistics-linreg"
                 , "stm-chans"
                 , "store-core"
                 , "streaming"
                 , "streaming-binary"
                 , "streaming-bytestring"
                 , "streaming-commons"
                 , "strict"
                 , "strict-concurrency"
                 , "string-conv"
                 , "string-conversions"
                 , "string-interpolate"
                 , "string-qq"
                 , "syb"
                 , "system-fileio"
                 , "system-filepath"
                 , "systemd"
                 , "tabular"
                 , "tagged"
                 , "tagsoup"
                 , "tar"
                 , "tar-conduit"
                 , "tasty < 1.4"                -- https://github.com/ocharles/tasty-rerun/issues/20
                 , "tasty-hedgehog"
                 , "tasty-hunit"
                 , "tasty-quickcheck"
                 , "tasty-rerun"
                 , "tdigest"
                 , "temporary"
                 , "terminal-size"
                 , "test-framework"
                 , "test-framework-hunit"
                 , "texmath"
                 , "text-conversions"
                 , "text-format"
                 , "text-icu"
                 , "text-manipulate"
                 , "text-metrics"
                 , "text-short"
                 , "text-zipper"
                 , "tf-random"
                 , "th-abstraction"
                 , "th-compat"
                 , "th-expand-syns"
                 , "th-lift"
                 , "th-lift-instances"
                 , "th-orphans"
                 , "th-reify-many"
                 , "th-utilities"
                 , "these"
                 , "threepenny-gui"
                 , "time-compat"
                 , "time-locale-compat"
                 , "time-manager"
                 , "time-units"
                 , "timeit"
                 , "timezone-olson"
                 , "timezone-series"
                 , "tls"
                 , "tls-session-manager"
                 , "topograph"
                 , "torrent"
                 , "transformers-base"
                 , "transformers-compat"
                 , "transformers-except"
                 , "tree-diff"
                 , "turtle"
                 , "type-equality"
                 , "typed-process"
                 , "uglymemo"
                 , "unbounded-delays"
                 , "unicode"
                 , "unicode-collation"
                 , "unicode-data"
                 , "unicode-transforms"
                 , "uniplate"
                 , "Unique"
                 , "unix-bytestring"
                 , "unix-compat"
                 , "unix-time"
                 , "unliftio"
                 , "unliftio-core"
                 , "unordered-containers"
                 , "uri-encode"
                 , "utf8-string"
                 , "utility-ht"
                 , "uuid"
                 , "uuid-types"
                 , "vault"
                 , "vector >=0.12.0.1 && <0.13"         -- can't update to 0.13.x because of aeson
                 , "vector-algorithms < 0.9"            -- keep tdigests building
                 , "vector-binary-instances"
                 , "vector-builder"
                 , "vector-th-unbox"
                 , "void"
                 , "vty"
                 , "wai"
                 , "wai-app-static"
                 , "wai-extra"
                 , "wai-logger"
                 , "warp"
                 , "warp-tls"
                 , "wcwidth"
                 , "websockets"
                 , "websockets-snap"
                 , "weeder < 2.3.0"
                 , "witherable"
                 , "wizards"
                 , "wl-pprint-annotated"
                 , "wl-pprint-text"
                 , "word-wrap"
                 , "word8"
                 , "wreq"
                 , "X11"
                 , "X11-xft"
                 , "x509"
                 , "x509-store"
                 , "x509-system"
                 , "x509-validation"
                 , "xml"
                 , "xml-conduit"
                 , "xml-hamlet"
                 , "xml-types"
                 , "xmobar"
                 , "xmonad"
                 , "xmonad-contrib"
                 , "xss-sanitize"
                 , "yaml"
                 , "yesod"
                 , "yesod-core"
                 , "yesod-form"
                 , "yesod-persistent"
                 , "yesod-static"
                 , "zip"
                 , "zip-archive"
                 , "zlib"
                 , "zlib-bindings"
                 , "zstd"
                 ]

flagList :: [(String,String)]
flagList =
  [ ("cabal-plan",                     "exe")

    -- Don't build hardware-specific optimizations into the binary based on what the
    -- build machine supports or doesn't support.
  , ("cryptonite",                     "-support_aesni -support_rdrand -support_blake2_sse")

    -- Don't use the bundled sqlite3 library.
  , ("direct-sqlite",                  "systemlib")

    -- dont optimize happy with happy ( dep on same package ..)
  , ("happy",                          "-bootstrap")

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
  , ("xmobar",                         "all_extensions")

    -- Enable additional features
  , ("idris",                          "ffi gmp")

    -- Disable dependencies we don't have.
  , ("invertible",                     "-hlist -piso")

    -- Use the system sqlite library rather than the bundled one.
  , ("persistent-sqlite",              "systemlib")

    -- Make sure we're building with the test suite enabled.
  , ("git-annex",                      "testsuite")

    -- Compile against the system library, not the one bundled in the Haskell
    -- source tarball.
  , ("cmark",                          "pkgconfig")

    -- Fix build with modern compilers.
  , ("cassava",                        "-bytestring--lt-0_10_4")

    -- Prefer the system's library over the bundled one.
  , ("libyaml",                        "system-libyaml")

    -- Configure a production-like build environment.
  , ("stack",                          "hide-dependency-versions disable-git-info supported-build")

    -- The command-line utility pulls in other dependencies.
  , ("aeson-pretty",                   "lib-only")

    -- Build the standalone executable for skylighting.
  , ("skylighting",                    "executable")

    -- Compile against system zstd library
  , ("zstd",                           "-standalone")
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

ghcCorePackages :: PackageSet
ghcCorePackages = [ "array-0.5.4.0"
                  , "base-4.14.3.0"
                  , "binary-0.8.8.0"
                  , "bytestring-0.10.12.0"
                  , "Cabal-3.2.1.0"
                  , "containers-0.6.5.1"
                  , "deepseq-1.4.4.0"
                  , "directory-1.3.6.0"
                  , "exceptions-0.10.4"
                  , "filepath-1.4.2.1"
                  , "ghc-8.10.3"
                  , "ghc-boot-8.10.7"
                  , "ghc-boot-th-8.10.7"
                  , "ghc-compact-0.1.0.0"
                  , "ghc-heap-8.10.7"
                  , "ghc-prim-0.6.1"
                  , "ghci-8.10.7"
                  , "haskeline-0.8.2"
                  , "hpc-0.6.1.0"
                  , "hsc2hs-0.68.7"
                  , "integer-gmp-1.0.3.0"
                  , "libiserv-8.10.7"
                  , "mtl-2.2.2"
                  , "parsec-3.1.14.0"
                  , "pretty-1.1.3.6"
                  , "process-1.6.13.2"
                  , "rts-1.0"
                  , "stm-2.5.0.1"
                  , "template-haskell-2.16.0.0"
                  , "terminfo-0.4.1.4"
                  , "text-1.2.4.1"
                  , "time-1.9.3"
                  , "transformers-0.5.6.2"
                  , "unix-2.7.2.2"
                  , "xhtml-3000.2.2.1"
                  ]

checkConsistency :: MonadFail m => PackageSetConfig -> m PackageSetConfig
checkConsistency pset@PackageSetConfig {..} = do
  let corePackagesInPackageSet = Map.keysSet packageSet `Set.intersection` Map.keysSet corePackages
  unless (Set.null corePackagesInPackageSet) $
    fail ("core packages listed in package set: " <> List.intercalate ", " (unPackageName <$> Set.toList corePackagesInPackageSet))
  pure pset
