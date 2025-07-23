{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Config.Ghc910x ( ghc910x ) where

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

ghc910x :: Action PackageSetConfig
ghc910x = do
  let compiler = "ghc-9.10.2"
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
--                    ]

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
constraintList = [ "adjunctions ^>= 4.4.2"
                 , "aeson ^>= 2.2.3.0"
                 , "aeson-pretty ^>= 0.8.10"
                 , "alex"
                 , "alsa-core ^>= 0.5.0.1"
                 , "alsa-mixer ^>= 0.3.0.1"
                 , "ansi-terminal ^>= 1.1.2"
                 , "ansi-terminal-types ^>= 1.1"
                 , "appar ^>= 0.1.8"
                 , "asn1-encoding ^>= 0.9.6"
                 , "asn1-parse ^>= 0.9.5"
                 , "asn1-types ^>= 0.3.4"
                 , "assoc ^>= 1.1.1"
                 , "async ^>= 2.2.5"
                 , "atomic-counter"
                 , "attoparsec ^>= 0.14.4"
                 , "attoparsec-aeson ^>= 2.2.2.0"
                 , "authenticate-oauth >=1.5 && <1.8"
                 , "auto-update ^>= 0.2.2"
                 , "aws ^>= 0.24.3"
                 , "base-compat ^>= 0.14.0"
                 , "base-orphans ^>= 0.9.2"
                 , "base-unicode-symbols ^>= 0.2.4.2"
                 , "base16-bytestring ^>= 1.0.2.0"
                 , "base64-bytestring ^>= 1.2.1.0"
                 , "basement ^>= 0.0.16"
                 , "bencode ^>= 0.6.1.1"
                 , "bifunctors ^>= 5.6.2"
                 , "bimap ^>= 0.5.0"
                 , "bitvec ^>= 1.1.5.0"
                 , "blaze-builder ^>= 0.4.2.3"
                 , "blaze-html ^>= 0.9.2.0"
                 , "blaze-markup ^>= 0.8.3.0"
                 , "bloomfilter ^>= 2.0.1.2"
                 , "boring ^>= 0.2.2"
                 , "brick >=2.1.1 && <2.3.2 || >2.3.2 && <2.5"
                 , "bsb-http-chunked ^>= 0.0.0.4"
                 , "byteable ^>= 0.1.1"
                 , "byteorder ^>= 1.0.4"
                 , "c2hs ^>= 0.28.8"
                 , "cabal-doctest"
                 , "cabal-install == 3.12.*"
                 , "cabal-install-solver == 3.12.*"
                 , "cabal-plan"
                 , "cabal2spec < 2.8"
                 , "cairo >=0.13"
                 , "call-stack ^>= 0.4.0"
                 , "case-insensitive ^>= 1.2.1.0"
                 , "cassava ^>= 0.5.3.2"
                 , "cassava-megaparsec ^>= 2.1.1"
                 , "cborg ^>= 0.2.10.0"
                 , "cereal ^>= 0.5.8.3"
                 , "character-ps ^>= 0.1"
                 , "citeproc"
                 , "clientsession ^>= 0.9.3.0"
                 , "clock ^>= 0.8.4"
                 , "cmdargs ^>= 0.10.22"
                 , "colour ^>= 2.3.6"
                 , "colourista ^>= 0.1.0.2"
                 , "commonmark ^>= 0.2.6.1"
                 , "commonmark-extensions ^>= 0.2.5.5"
                 , "commonmark-pandoc ^>= 0.2.2.2"
                 , "commutative-semigroups ^>= 0.2.0.1"
                 , "comonad ^>= 5.0.8"
                 , "concurrent-output ^>= 1.10.21"
                 , "conduit ^>= 1.3.6"
                 , "conduit-extra ^>= 1.3.6"
                 , "config-ini ^>= 0.2.7.0"
                 , "constraints ^>= 0.14.2"
                 , "contravariant ^>= 1.5.5"
                 , "control-monad-free ^>= 0.6.2"
                 , "cookie ^>= 0.5.0"
                 , "cpphs ^>= 1.20.9.1"
                 , "crypto-api ^>= 0.13.3"
                 , "crypto-pubkey-types >=0.1 && <0.5"
                 , "crypto-token ^>= 0.1.2"
                 , "cryptohash-md5 ^>= 0.11.101.0"
                 , "cryptohash-sha1 ^>= 0.11.101.0"
                 , "cryptohash-sha256 ^>= 0.11.102.1"
                 , "crypton ^>= 1.0.1"
                 , "crypton-connection ^>= 0.4.2"
                 , "crypton-socks >= 0.6"
                 , "crypton-x509 ^>= 1.7.7"
                 , "crypton-x509-store ^>= 1.6.9"
                 , "crypton-x509-system ^>= 1.6.7"
                 , "crypton-x509-validation ^>= 1.6.13"
                 , "cryptonite ^>= 0.30"
                 , "cryptonite-conduit ^>= 0.2.2"
                 , "css-text ^>= 0.1.3.0"
                 , "csv ^>= 0.1.2"
                 , "data-clist ^>= 0.2"
                 , "data-default ^>= 0.8.0.0"
                 , "data-default-class ^>= 0.2.0.0"
                 , "data-fix ^>= 0.3.4"
                 , "DAV ^>= 1.3.4"
                 , "dbus ^>= 1.3.9"
                 , "dec ^>= 0.0.6"
                 , "Decimal ^>= 0.5.2"
                 , "deriving-aeson ^>= 0.2.9"
                 , "Diff ^>= 1.0.2"
                 , "digest ^>= 0.0.2.1"
                 , "directory-ospath-streaming >=0.2.1 && <0.3"
                 , "disk-free-space ^>= 0.1.0.1"
                 , "distributive ^>= 0.6.2.1"
                 , "djot ^>= 0.1.2.2"
                 , "dlist ^>= 1.0"
                 , "doclayout ^>= 0.5"
                 , "doctemplates ^>= 0.11.0.1"
                 , "easy-file ^>= 0.2.5"
                 , "echo ^>= 0.1.4"
                 , "ed25519 ^>= 0.0.5.0"
                 , "edit-distance ^>= 0.2.2.1"
                 , "email-validate ^>= 2.3.2.21"
                 , "emojis ^>= 0.1.4.1"
                 , "encoding ^>= 0.10"
                 , "entropy ^>= 0.4.1.10"
                 , "extensible-exceptions ^>= 0.1.1.4"
                 , "extra ^>= 1.8"
                 , "fast-logger ^>= 3.2.5"
                 , "fdo-notify ^>= 0.3.1"
                 , "feed ^>= 1.3.2.1"
                 , "fgl ^>= 5.8.3.0"
                 , "file-embed ^>= 0.0.16.0"
                 , "file-io <0.2"
                 , "filepath-bytestring ^>= 1.5.2.0.0"
                 , "filepattern ^>= 0.1.3"
                 , "foldl ^>= 1.4.17"
                 , "free ^>= 5.2"
                 , "fsnotify ^>= 0.4.1.0"
                 , "generically ^>= 0.1.1"
                 , "generics-sop ^>=0.5.1"
                 , "git-annex"
                 , "git-lfs ^>= 1.2.2"
                 , "githash ^>= 0.1.7.0"
                 , "gitrev ^>= 1.3.1"
                 , "glib >=0.13.0.0 && <0.14"
                 , "Glob ^>= 0.10.2"
                 , "gridtables ^>= 0.1.0.0"
                 , "gtk2hs-buildtools"
                 , "hackage-security ^>= 0.6.2.6"
                 , "haddock-library ^>= 1.11.0"
                 , "half ^>= 0.3.1"
                 , "happy == 1.20.1.1"     -- TODO: happy 2.1.3 doesn't compile
                 , "hashable ^>= 1.5.0.0"
                 , "hashtables ^>= 1.4.0"
                 , "haskell-lexer ^>= 1.1.2"
                 , "haskell-src-exts <1.24"
                 , "haskell-src-meta <0.9"
                 , "hinotify ^>= 0.4.1"
                 , "hjsmin ^>= 0.2.1"
                 , "hledger"
                 , "hledger-interest"
                 , "hledger-lib"
                 , "hledger-ui"
                 , "hourglass ^>= 0.2.12"
                 , "hscolour ^>= 1.25"
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
                 , "HsYAML ^>= 0.2.1.4"
                 , "html ^>= 1.0.1.2"
                 , "HTTP ^>= 4000.4.1"
                 , "http-api-data ^>= 0.6.1"
                 , "http-client ^>= 0.7.17"
                 , "http-client-restricted ^>= 0.1.0"
                 , "http-client-tls ^>= 0.3.6.4"
                 , "http-conduit ^>= 2.3.9.1"
                 , "http-date ^>= 0.0.11"
                 , "http-media ^>= 0.8.1.1"
                 , "http-semantics ^>= 0.3.0"
                 , "http-types ^>= 0.12.4"
                 , "http2 ^>= 5.3.6"
                 , "HUnit ^>= 1.6.2.0"
                 , "IfElse ^>= 0.85"
                 , "ilist ^>= 0.4.0.1"
                 , "indexed-profunctors ^>= 0.1.1.1"
                 , "indexed-traversable ^>= 0.1.4"
                 , "indexed-traversable-instances ^>= 0.1.2"
                 , "integer-conversion ^>= 0.1.1"
                 , "integer-logarithms ^>= 1.0.3.1"
                 , "invariant ^>= 0.6.3"
                 , "iproute ^>= 1.7.15"
                 , "ipynb ^>= 0.2"
                 , "isocline ^>= 1.0.9"
                 , "iwlib"
                 , "jira-wiki-markup ^>= 1.5.1"
                 , "JuicyPixels ^>= 3.3.9"
                 , "kan-extensions ^>= 5.2.6"
                 , "language-c ^>= 0.10.0"
                 , "language-docker ^>= 13.0.0"
                 , "language-javascript ^>= 0.7.1.0"
                 , "lens ^>= 5.3.2"
                 , "libmpd ^>= 0.10.0.1"
                 , "libyaml ^>= 0.1.4"
                 , "lift-type ^>= 0.1.2.0"
                 , "lifted-base ^>= 0.2.3.12"
                 , "lpeg ^>= 1.1.0"
                 , "lua ^>= 2.3.3"
                 , "lucid ^>= 2.11.20230408"
                 , "lukko ^>= 0.1.2"
                 , "magic ^>= 1.1"
                 , "math-functions ^>= 0.3.4.4"
                 , "megaparsec ^>= 9.6.1"
                 , "memory ^>= 0.18.0"
                 , "microlens ^>= 0.4.13.1"
                 , "microlens-ghc ^>= 0.4.14.3"
                 , "microlens-mtl ^>= 0.2.0.3"
                 , "microlens-platform ^>= 0.4.3.5"
                 , "microlens-th ^>= 0.4.3.15"
                 , "mime-types ^>= 0.1.2.0"
                 , "mmorph ^>= 1.2.0"
                 , "modern-uri ^>= 0.3"
                 , "monad-control ^>= 1.0.3.1"
                 , "monad-logger ^>= 0.3.40"
                 , "monad-loops ^>= 0.4.3"
                 , "mono-traversable ^>= 1.0.20.0"
                 , "monoid-subclasses ^>= 1.2.5.1"
                 , "mountpoints ^>= 1.0.2"
                 , "netlink ^>= 1.1.1.0"
                 , "network ^>= 3.2.6.0"
                 , "network-bsd ^>= 2.8.1.0"
                 , "network-byte-order ^>= 0.1.7"
                 , "network-control ^>= 0.1.3"
                 , "network-info ^>= 0.2.1"
                 , "network-multicast ^>= 0.3.2"
                 , "network-uri ^>= 2.6.4.2"
                 , "old-locale ^>= 1.0.0.7"
                 , "old-time ^>= 1.1.0.4"
                 , "OneTuple ^>= 0.4.2"
                 , "Only ^>= 0.1"
                 , "open-browser ^>= 0.2.1.0"
                 , "optics-core ^>= 0.4.1.1"
                 , "optparse-applicative ^>= 0.18.1.0"
                 , "ordered-containers ^>= 0.2.4"
                 , "pager ^>= 0.1.1.0"
                 , "pandoc"
                 , "pandoc-cli"
                 , "pandoc-lua-engine"
                 , "pandoc-lua-marshal"
                 , "pandoc-server"
                 , "pandoc-types"
                 , "pango >=0.13"
                 , "parallel ^>= 3.2.2.0"
                 , "parsec-numbers ^>= 0.1.0"
                 , "parser-combinators ^>= 1.3.0"
                 , "path-pieces ^>= 0.2.1"
                 , "pem ^>= 0.2.4"
                 , "persistent >=2.15.2.0 && <3"
                 , "persistent-sqlite ^>= 2.13.3.1"
                 , "polyparse ^>= 1.13"
                 , "postgresql-libpq >=0.10.0.0 && <0.12"
                 , "postgresql-libpq-configure ^>=0.11"
                 , "postgresql-simple"
                 , "pretty-hex ^>= 1.1"
                 , "pretty-show ^>= 1.10"
                 , "pretty-simple ^>= 4.1.2.0"
                 , "prettyprinter ^>= 1.7.1"
                 , "prettyprinter-ansi-terminal ^>= 1.1.3"
                 , "primes ^>= 0.2.1.0"
                 , "primitive ^>= 0.9.0.0"
                 , "profunctors ^>= 5.6.2"
                 , "psqueues ^>= 0.2.8.0"
                 , "puresat ^>= 0.1"
                 , "QuickCheck ^>= 2.15.0.1"
                 , "quote-quot ^>= 0.2.1.0"
                 , "random ^>= 1.2.1.2"
                 , "raw-strings-qq"
                 , "recv ^>= 0.1.0"
                 , "refact ^>= 0.3.0.2"
                 , "reflection ^>= 2.1.8"
                 , "regex-base ^>= 0.94.0.2"
                 , "regex-compat ^>= 0.95.2.1"
                 , "regex-posix ^>= 0.96.0.1"
                 , "regex-tdfa ^>= 1.3.2.2"
                 , "replace-megaparsec ^>= 1.5.0.1"
                 , "req ^>= 3.13.4"
                 , "resolv ^>= 0.2.0.2"
                 , "resource-pool ^>= 0.4.0.0"
                 , "resourcet ^>= 1.3.0"
                 , "retry"
                 , "RSA >=2.0 && <2.5"
                 , "safe ^>= 0.3.21"
                 , "safe-exceptions ^>= 0.1.7.4"
                 , "SafeSemaphore ^>= 0.10.1"
                 , "sandi ^>= 0.5"
                 , "scientific ^>= 0.3.8.0"
                 , "securemem ^>= 0.1.10"
                 , "semialign ^>= 1.3.1"
                 , "semigroupoids ^>= 6.0.1"
                 , "semigroups ^>= 0.20"
                 , "semver ^>= 0.4.0.1"
                 , "serialise ^>= 0.2.6.1"
                 , "servant ^>= 0.20.2"
                 , "servant-client ^>= 0.20.2"
                 , "servant-client-core ^>= 0.20.2"
                 , "servant-server ^>= 0.20.2"
                 , "setenv ^>= 0.1.1.3"
                 , "setlocale ^>= 1.0.0.10"
                 , "SHA >=1.4 && <1.7"
                 , "shakespeare ^>= 2.1.0.1"
                 , "ShellCheck"
                 , "silently ^>= 1.2.5.3"
                 , "simple-sendfile ^>= 0.2.32"
                 , "singleton-bool ^>= 0.1.8"
                 , "skein ^>= 1.0.9.4"
                 , "skylighting ^>= 0.14.3"
                 , "skylighting-core ^>= 0.14.3"
                 , "skylighting-format-ansi ^>= 0.1"
                 , "skylighting-format-blaze-html ^>= 0.1.1.2"
                 , "skylighting-format-context ^>= 0.1.0.2"
                 , "skylighting-format-latex ^>= 0.1"
                 , "skylighting-format-typst >=0.1 && <0.2"
                 , "socks ^>= 0.6.1"
                 , "some ^>= 1.0.6"
                 , "sop-core ^>= 0.5.0.2"
                 , "spdx ^>= 1.1"
                 , "split ^>= 0.2.5"
                 , "splitmix ^>= 0.1.0.5"
                 , "StateVar ^>= 1.2.2"
                 , "stm-chans ^>= 3.0.0.9"
                 , "streaming-commons ^>= 0.2.2.6"
                 , "strict ^>= 0.5.1"
                 , "string-interpolate"
                 , "syb ^>= 0.7.2.4"
                 , "tabular ^>= 0.2.2.8"
                 , "tagged ^>= 0.8.8"
                 , "tagsoup ^>= 0.14.8"
                 , "tar ^>= 0.6.3.0"
                 , "tasty ^>= 1.5.2"
                 , "tasty-hunit ^>= 0.10.2"
                 , "tasty-quickcheck ^>= 0.11"
                 , "tasty-rerun ^>= 1.1.19"
                 , "temporary ^>= 1.3"
                 , "terminal-size ^>= 0.3.4"
                 , "texmath >=0.12.10.2 && <0.13"
                 , "text-ansi ^>= 0.3.0.1"
                 , "text-builder-linear ^>= 0.1.3"
                 , "text-conversions ^>= 0.3.1.1"
                 , "text-icu ^>= 0.8.0.5"
                 , "text-iso8601 ^>= 0.1.1"
                 , "text-short ^>= 0.1.6"
                 , "text-zipper ^>= 0.13"
                 , "th-abstraction ^>= 0.7.0.0"
                 , "th-compat ^>= 0.1.5"
                 , "th-expand-syns"
                 , "th-lift ^>= 0.8.5"
                 , "th-lift-instances ^>= 0.1.20"
                 , "th-orphans >=0.12 && <0.14"
                 , "th-reify-many >=0.1.9 && <0.2"
                 , "these ^>= 1.2.1"
                 , "time-compat ^>= 1.9.7"
                 , "time-locale-compat ^>= 0.1.1.5"
                 , "time-manager ^>= 0.2"
                 , "timeit ^>= 2.0"
                 , "timerep ^>= 2.1.0.0"
                 , "timezone-olson ^>= 0.2.1"
                 , "timezone-series ^>= 0.1.13"
                 , "tls < 2.1.9"        -- TODO: the new version needs random-1.3.x
                 , "tls-session-manager ^>= 0.0.7"
                 , "toml-parser ^>= 2.0.1.0"
                 , "topograph ^>= 1.0.1"
                 , "torrent ^>= 10000.1.3"
                 , "transformers-base ^>= 0.4.6"
                 , "transformers-compat ^>= 0.7.2"
                 , "typed-process ^>= 0.2.12.0"
                 , "typst >=0.8 && <0.9"
                 , "typst-symbols >=0.1.8 && <0.1.9"
                 , "uglymemo ^>= 0.1.0.1"
                 , "unbounded-delays ^>= 0.1.1.1"
                 , "unicode-collation ^>= 0.1.3.6"
                 , "unicode-data ^>= 0.6.0"
                 , "unicode-transforms ^>= 0.4.0.1"
                 , "uniplate ^>= 1.6.13"
                 , "unix-compat ^>= 0.7.3"
                 , "unix-time ^>= 0.4.16"
                 , "unliftio ^>= 0.2.25.0"
                 , "unliftio-core ^>= 0.2.1.0"
                 , "unordered-containers ^>= 0.2.20"
                 , "utf8-string ^>= 1.0.2"
                 , "utility-ht ^>= 0.0.17.2"
                 , "uuid ^>= 1.3.16"
                 , "uuid-types ^>= 1.0.6"
                 , "vault ^>= 0.3.1.5"
                 , "vector ^>= 0.13.2.0"
                 , "vector-algorithms ^>= 0.9.0.2"
                 , "vector-stream ^>= 0.1.0.1"
                 , "void ^>= 0.7.3"
                 , "vty ^>= 6.2"
                 , "vty-crossplatform ^>= 0.4.0.0"
                 , "vty-unix ^>= 0.2.0.0"
                 , "wai ^>= 3.2.4"
                 , "wai-app-static ^>= 3.1.9"
                 , "wai-cors ^>= 0.2.7"
                 , "wai-extra < 3.1.17"         -- TODO: update breaks yesod-core, see https://github.com/yesodweb/yesod/issues/1854
                 , "wai-logger ^>= 2.5.0"
                 , "warp ^>= 3.4.4"
                 , "warp-tls ^>= 3.4.11"
                 , "witherable ^>= 0.5"
                 , "wizards ^>= 1.0.3"
                 , "word-wrap ^>= 0.5"
                 , "word8 ^>= 0.1.3"
                 , "X11 ^>= 1.10.3"
                 , "X11-xft ^>= 0.3.4"
                 , "xml ^>= 1.3.14"
                 , "xml-conduit ^>= 1.9.1.4"
                 , "xml-hamlet ^>= 0.5.0.2"
                 , "xml-types ^>= 0.3.8"
                 , "xmobar"
                 , "xmonad"
                 , "xmonad-contrib"
                 , "xss-sanitize ^>= 0.3.7.2"
                 , "yaml ^>= 0.11.11.2"
                 , "yesod ^>= 1.6.2.1"
                 , "yesod-core ^>= 1.6.26.0"
                 , "yesod-form ^>= 1.7.9"
                 , "yesod-persistent ^>= 1.6.0.8"
                 , "yesod-static ^>= 1.6.1.0"
                 , "zip-archive ^>= 0.4.3.2"
                 , "zlib ^>= 0.7.1.0"
                 ]

flagList :: [(String,String)]
flagList =
  [ ("cabal-plan",                     "+exe")

    -- Don't build hardware-specific optimizations into the binary based on what the
    -- build machine supports or doesn't support.
  , ("cryptonite",                     "-support_aesni -support_rdrand -support_sse")

    -- Don't use the bundled sqlite3 library.
  , ("direct-sqlite",                  "+systemlib")

    -- dont optimize happy with happy ( dep on same package ..)
  , ("happy",                          "-bootstrap")

    -- Build the standalone executable and prefer pcre-light, which uses the system
    -- library rather than a bundled copy.
  , ("highlighting-kate",              "+executable +pcre-light")

    -- Don't use the bundled sass library.
  , ("hlibsass",                       "+externalLibsass")

    -- Use the bundled lua library. People expect this package to provide LUA
    -- 5.3, but we don't have that yet in openSUSE.
  , ("hslua",                          "-system-lua")

    -- Allow compilation without having Nix installed.
  , ("nix-paths",                      "+allow-relative-paths")

    -- Build the standalone executable.
  , ("texmath",                        "+executable")

    -- Enable almost all extensions.
  , ("xmobar",                         "+all_extensions")

    -- Enable additional features
  , ("idris",                          "+ffi +gmp")

    -- Disable dependencies we don't have.
  , ("invertible",                     "-hlist -piso")

    -- Use the system sqlite library rather than the bundled one.
  , ("persistent-sqlite",              "+systemlib")

    -- Make sure we're building with the test suite enabled.
  , ("git-annex",                      "+Assistant +Pairing +Production +TorrentParser +MagicMime +Crypton +Servant -Benchmark +Dbus")

    -- Compile against the system library, not the one bundled in the Haskell
    -- source tarball.
  , ("cmark",                          "+pkgconfig")

    -- Fix build with modern compilers.
  , ("cassava",                        "-bytestring--lt-0_10_4")

    -- Prefer the system's library over the bundled one.
  , ("libyaml",                        "+system-libyaml")

    -- Don't install example files.
  , ("yaml",                            "-no-examples -no-exe")

    -- Configure a production-like build environment.
  , ("stack",                          "+hide-dependency-versions +disable-git-info +supported-build")

    -- The command-line utility pulls in other dependencies.
  , ("aeson-pretty",                   "+lib-only")

    -- Build the standalone executables for these packages.
  , ("skylighting",                    "+executable")
  , ("citeproc",                       "+executable")

    -- Compile against system zstd library
  , ("zstd",                           "-standalone")

    -- Static linking breaks the build.
  , ("hadolint",                       "-static")

    -- Enable LUA support.
  , ("pandoc-cli",                     "+lua +server")

    -- Disable use of -march=native during compilation.
  , ("hashable",                       "-arch-native")

    -- Don't build and install the 'example' binary.
  , ("open-browser",                   "-example")
  ]

readFlagAssignents :: [(String,String)] -> [(PackageName,FlagAssignment)]
readFlagAssignents xs = [ (fromJust (simpleParse name), readFlagList (words assignments)) | (name,assignments) <- xs ]

readFlagList :: [String] -> FlagAssignment
readFlagList = mkFlagAssignment . map (tagWithValue . specifyPlus . noMinusF)
  where
    tagWithValue ('-':fname) = (mkFlagName (lowercase fname), False)
    tagWithValue fname       = (mkFlagName (lowercase fname), True)

    noMinusF :: String -> String
    noMinusF ('-':'f':_) = error "don't use '-f' in flag assignments; just use the flag's name"
    noMinusF x           = x

    specifyPlus :: String -> String
    specifyPlus ""           = error "specifyPlus: invalid empty Cabal flag"
    specifyPlus x@(c:_)
      | c == '-' || c == '+' = x
      | otherwise            = error ("specifyPlus: flag " ++ show x ++ " must start with '-' or '+'")

ghcCorePackages :: PackageSet
ghcCorePackages = [ "array-0.5.8.0"
                  , "base-4.20.1.0"
                  , "binary-0.8.9.3"
                  , "bytestring-0.12.2.0"
                  , "Cabal-3.12.1.0"
                  , "Cabal-syntax-3.12.1.0"
                  , "containers-0.7"
                  , "deepseq-1.5.0.0"
                  , "directory-1.3.8.5"
                  , "exceptions-0.10.9"
                  , "filepath-1.5.4.0"
                  , "ghc-9.10.2"
                  , "ghc-bignum-1.3"
                  , "ghc-boot-9.10.2"
                  , "ghc-boot-th-9.10.2"
                  , "ghc-compact-0.1.0.0"
                  , "ghc-experimental-9.1002.0"
                  , "ghc-heap-9.10.2"
                  , "ghc-internal-9.1002.0"
                  , "ghc-platform-0.1.0.0"
                  , "ghc-prim-0.12.0"
                  , "ghc-toolchain-0.1.0.0"
                  , "ghci-9.10.2"
                  , "haskeline-0.8.2.1"
                  , "hpc-0.7.0.2"
                  , "integer-gmp-1.1"
                  , "mtl-2.3.1"
                  , "os-string-2.0.4"
                  , "parsec-3.1.18.0"
                  , "pretty-1.1.3.6"
                  , "process-1.6.25.0"
                  , "rts-1.0.2"
                  , "semaphore-compat-1.0.0"
                  , "stm-2.5.3.1"
                  , "system-cxx-std-lib-1.0"
                  , "template-haskell-2.22.0.0"
                  , "terminfo-0.4.1.6"
                  , "text-2.1.2"
                  , "time-1.12.2"
                  , "transformers-0.6.1.1"
                  , "unix-2.8.6.0"
                  , "xhtml-3000.2.2.1"

                  -- This package is a part of ghc-compiler."
                  , "hsc2hs-0.68.10"

                  -- This is a bad hack. We have some Hackage packages that are
                  -- empty and that we don't want to package, but still opther
                  -- libraries depend on them being available. So we list them
                  -- as core packages to make the Cabal resolver happy even
                  -- though we actually don't use them at build time.
                  , "bytestring-builder-0.10.8.2.0"     -- now part of bytestring
                  , "persistent-template-2.12.0.0"      -- now part of persistent
                  , "mtl-compat-0.2.2"                  -- empty library
                  ]

-- TODO: Detect at compile-time or run-time if constraintList contains
-- duplicate entries.

checkConsistency :: MonadFail m => PackageSetConfig -> m PackageSetConfig
checkConsistency pset@PackageSetConfig {..} = do
  let corePackagesInPackageSet = Map.keysSet packageSet `Set.intersection` Map.keysSet corePackages
  unless (Set.null corePackagesInPackageSet) $
    fail ("core packages listed in package set: " <> List.intercalate ", " (unPackageName <$> Set.toList corePackagesInPackageSet))
  pure pset
