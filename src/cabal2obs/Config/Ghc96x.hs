{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Config.Ghc96x ( ghc96x ) where

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

ghc96x :: Action PackageSetConfig
ghc96x = do
  let compiler = "ghc-9.6.1"
      flagAssignments = fromList (readFlagAssignents flagList)
      forcedExectables = forcedExectableNames
      corePackages = ghcCorePackages
  packageSet <- fromList <$>
                  forM (toList constraintList) (\(pn,vr) ->
                    (,) pn <$> askOracle (PackageVersionConstraint pn vr))
  checkConsistency (PackageSetConfig {..})

-- targetPackages :: ConstraintSet
-- targetPackages   = [ "alex"
--                    , "cabal-install"
--                    , "cabal2spec"
--                    , "cabal-plan"
--                    , "git-annex"
--                    , "happy"
--                    , "hledger", "hledger-ui", "hledger-interest"
--                    , "hlint"
--                    , "pandoc", "pandoc-cli", "pandoc-lua-engine", "pandoc-server"
--                    , "citeproc"
--                    , "postgresql-simple"        -- needed by emu-incident-report
--                    , "ShellCheck"
--                    , "xmobar"
--                    , "xmonad"
--                    , "xmonad-contrib"
--                    , "hadolint"
--                    ]
--
-- resolveConstraints :: String
-- resolveConstraints = unwords ["cabal", "install", "--dry-run", "--minimize-conflict-set", constraints, flags, pkgs]
--   where
--     pkgs = unwords (display <$> Map.keys targetPackages)
--     constraints = "--constraint=" <> List.intercalate " --constraint=" (show <$> environment)
--     environment = display . uncurry PackageVersionConstraint <$> toList targetPackages
--     flags = unwords [ "--constraint=" <> show (unwords [unPackageName pn, flags'])
--                     | pn <- Map.keys targetPackages
--                     , Just flags' <- [lookup (unPackageName pn) flagList]
--                     ]

constraintList :: ConstraintSet
constraintList = [ "adjunctions"
                 , "aeson"
                 , "aeson-pretty"
                 , "alex"
                 , "alsa-core"
                 , "alsa-mixer"
                 , "ansi-terminal"
                 , "ansi-terminal-types"
                 , "ansi-wl-pprint"
                 , "appar"
                 , "asn1-encoding"
                 , "asn1-parse"
                 , "asn1-types"
                 , "assoc"
                 , "async"
                 , "attoparsec"
                 , "attoparsec-iso8601"
                 , "auto-update"
                 , "base-compat"
                 , "base-compat-batteries"
                 , "base-orphans"
                 , "base-unicode-symbols"
                 , "base16-bytestring"
                 , "base64"
                 , "base64-bytestring"
                 , "basement"
                 , "bifunctors"
                 , "bitvec"
                 , "blaze-builder"
                 , "blaze-html"
                 , "blaze-markup"
                 , "boring"
                 , "bsb-http-chunked"
                 , "byteorder"
                 , "c2hs"
                 , "cabal-doctest"
                 , "cabal2spec"
                 , "cairo"
                 , "call-stack"
                 , "case-insensitive"
                 , "cereal"
                 , "citeproc"
                 , "clock"
                 , "cmdargs"
                 , "colour"
                 , "commonmark"
                 , "commonmark-extensions"
                 , "commonmark-pandoc"
                 , "comonad"
                 , "conduit"
                 , "conduit-extra"
                 , "connection"
                 , "constraints"
                 , "contravariant"
                 , "cookie"
                 , "cpphs"
                 , "cryptonite"
                 , "data-default"
                 , "data-default-class"
                 , "data-default-instances-containers"
                 , "data-default-instances-dlist"
                 , "data-default-instances-old-locale"
                 , "data-fix"
                 , "dbus"
                 , "dec"
                 , "deriving-aeson"
                 , "digest"
                 , "distributive"
                 , "dlist"
                 , "doclayout"
                 , "doctemplates"
                 , "easy-file"
                 , "emojis"
                 , "extensible-exceptions"
                 , "extra"
                 , "fast-logger"
                 , "file-embed"
                 , "filepattern"
                 , "free"
                 , "generically"
                 , "ghc-lib-parser"
                 , "ghc-lib-parser-ex"
                 , "glib"
                 , "Glob"
                 , "gridtables"
                 , "gtk2hs-buildtools"
                 , "haddock-library"
                 , "happy <1.21.0 || >1.21.0"
                 , "hashable"
                 , "hashtables"
                 , "haskell-lexer"
                 , "haskell98"
                 , "hinotify"
                 , "hlint"
                 , "hourglass"
                 , "hsc2hs"
                 , "hscolour"
                 , "hslua"
                 , "hslua-aeson"
                 , "hslua-classes"
                 , "hslua-cli"
                 , "hslua-core"
                 , "hslua-list"
                 , "hslua-marshalling"
                 , "hslua-module-doclayout"
                 , "hslua-module-path"
                 , "hslua-module-system"
                 , "hslua-module-text"
                 , "hslua-module-version"
                 , "hslua-module-zip"
                 , "hslua-objectorientation"
                 , "hslua-packaging"
                 , "hslua-repl"
                 , "hslua-typing"
                 , "http-api-data"
                 , "http-client"
                 , "http-client-tls"
                 , "http-conduit"
                 , "http-date"
                 , "http-media"
                 , "http-types"
                 , "http2"
                 , "HUnit"
                 , "indexed-traversable"
                 , "indexed-traversable-instances"
                 , "integer-logarithms"
                 , "invariant"
                 , "iproute"
                 , "ipynb"
                 , "isocline"
                 , "jira-wiki-markup"
                 , "JuicyPixels"
                 , "kan-extensions"
                 , "language-c"
                 , "lens"
                 , "libmpd"
                 , "libyaml"
                 , "lpeg"
                 , "lua"
                 , "memory"
                 , "mime-types"
                 , "mmorph"
                 , "monad-control"
                 , "monad-loops"
                 , "mono-traversable"
                 , "netlink"
                 , "network"
                 , "network-byte-order"
                 , "network-uri"
                 , "old-locale"
                 , "old-time"
                 , "OneTuple"
                 , "optparse-applicative"
                 , "pandoc"
                 , "pandoc-cli"
                 , "pandoc-lua-engine"
                 , "pandoc-lua-marshal"
                 , "pandoc-server"
                 , "pandoc-types"
                 , "pango"
                 , "parallel"
                 , "parsec-numbers"
                 , "pem"
                 , "polyparse"
                 , "pretty-hex"
                 , "pretty-show"
                 , "primitive"
                 , "profunctors"
                 , "psqueues"
                 , "QuickCheck"
                 , "random"
                 , "recv"
                 , "refact"
                 , "reflection"
                 , "regex-base"
                 , "regex-compat"
                 , "regex-posix"
                 , "resourcet"
                 , "safe"
                 , "safe-exceptions"
                 , "scientific"
                 , "semialign"
                 , "semigroupoids"
                 , "semigroups"
                 , "servant"
                 , "servant-server"
                 , "setlocale"
                 , "SHA"
                 , "simple-sendfile"
                 , "singleton-bool"
                 , "skylighting"
                 , "skylighting-core"
                 , "skylighting-format-ansi"
                 , "skylighting-format-blaze-html"
                 , "skylighting-format-context"
                 , "skylighting-format-latex"
                 , "socks"
                 , "some"
                 , "sop-core"
                 , "split"
                 , "splitmix"
                 , "StateVar"
                 , "streaming-commons"
                 , "strict"
                 , "string-conversions"
                 , "syb"
                 , "tagged"
                 , "tagsoup"
                 , "temporary"
                 , "texmath"
                 , "text-conversions"
                 , "text-short"
                 , "th-abstraction"
                 , "th-compat"
                 , "th-lift"
                 , "th-lift-instances"
                 , "these"
                 , "time-compat"
                 , "time-manager"
                 , "timezone-olson"
                 , "timezone-series"
                 , "tls"
                 , "transformers-base"
                 , "transformers-compat"
                 , "type-equality"
                 , "typed-process"
                 , "unicode-collation"
                 , "unicode-data"
                 , "unicode-transforms"
                 , "uniplate"
                 , "unix-compat"
                 , "unix-time"
                 , "unliftio"
                 , "unliftio-core"
                 , "unordered-containers"
                 , "utf8-string"
                 , "uuid-types"
                 , "vault"
                 , "vector"
                 , "vector-algorithms"
                 , "vector-stream"
                 , "void"
                 , "wai"
                 , "wai-app-static"
                 , "wai-cors"
                 , "wai-extra"
                 , "wai-logger"
                 , "warp"
                 , "witherable"
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
  [ ("cabal-plan",                     "exe")

    -- Don't build hardware-specific optimizations into the binary based on what the
    -- build machine supports or doesn't support.
  , ("cryptonite",                     "-support_aesni -support_rdrand -support_sse")

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

    -- Static linking breaks the build.
  , ("hadolint",                       "-static")
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
ghcCorePackages = [ "array-0.5.5.0"
                  , "base-4.18.0.0"
                  , "binary-0.8.9.1"
                  , "bytestring-0.11.4.0"
                  , "Cabal-3.10.1.0"
                  , "Cabal-syntax-3.10.1.0"
                  , "containers-0.6.7"
                  , "deepseq-1.4.8.1"
                  , "directory-1.3.8.1"
                  , "exceptions-0.10.7"
                  , "filepath-1.4.100.1"
                  , "ghc-9.6.1"
                  , "ghc-bignum-1.3"
                  , "ghc-boot-9.6.1"
                  , "ghc-boot-th-9.6.1"
                  , "ghc-compact-0.1.0.0"
                  , "ghc-heap-9.6.1"
                  , "ghc-prim-0.10.0"
                  , "ghci-9.6.1"
                  , "haskeline-0.8.2.1"
                  , "hpc-0.6.2.0"
                  , "integer-gmp-1.1"
                  , "libiserv-9.6.1"
                  , "mtl-2.3.1"
                  , "parsec-3.1.16.1"
                  , "pretty-1.1.3.6"
                  , "process-1.6.17.0"
                  , "rts-1.0.2"
                  , "stm-2.5.1.0"
                  , "system-cxx-std-lib-1.0"
                  , "template-haskell-2.20.0.0"
                  , "terminfo-0.4.1.6"
                  , "text-2.0.2"
                  , "time-1.12.2"
                  , "transformers-0.6.1.0"
                  , "unix-2.8.1.0"
                  , "xhtml-3000.2.2.1"
                  ]

checkConsistency :: MonadFail m => PackageSetConfig -> m PackageSetConfig
checkConsistency pset@PackageSetConfig {..} = do
  let corePackagesInPackageSet = Map.keysSet packageSet `Set.intersection` Map.keysSet corePackages
  unless (Set.null corePackagesInPackageSet) $
    fail ("core packages listed in package set: " <> List.intercalate ", " (unPackageName <$> Set.toList corePackagesInPackageSet))
  pure pset
