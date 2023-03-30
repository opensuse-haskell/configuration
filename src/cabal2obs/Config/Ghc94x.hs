{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Config.Ghc94x ( ghc94x ) where

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

ghc94x :: Action PackageSetConfig
ghc94x = do
  let compiler = "ghc-9.4.4"
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
                 , "aeson ^>= 2.1.1.0"
                 , "aeson-pretty ^>= 0.8.9"
                 , "alex ^>= 3.2.7.1"
                 , "alsa-core ^>= 0.5.0.1"
                 , "alsa-mixer ^>= 0.3.0"
                 , "ansi-terminal ^>= 0.11.4"
                 , "ansi-terminal-types ^>= 0.11.4"
                 , "ansi-wl-pprint ^>= 0.6.9"
                 , "appar ^>= 0.1.8"
                 , "asn1-encoding ^>= 0.9.6"
                 , "asn1-parse ^>= 0.9.5"
                 , "asn1-types ^>= 0.3.4"
                 , "assoc ^>= 1.0.2"
                 , "async ^>= 2.2.4"
                 , "attoparsec ^>= 0.14.4"
                 , "attoparsec-iso8601 ^>= 1.1.0.0"
                 , "auto-update ^>= 0.1.6"
                 , "aws ^>= 0.23"
                 , "base-compat ^>= 0.12.2"
                 , "base-compat-batteries ^>= 0.12.2"
                 , "base-orphans ^>= 0.8.7"
                 , "base-unicode-symbols ^>= 0.2.4.2"
                 , "base16-bytestring ^>= 1.0.2.0"
                 , "base64 ^>= 0.4.2.4"
                 , "base64-bytestring ^>= 1.2.1.0"
                 , "basement ^>= 0.0.15"
                 , "bencode ^>= 0.6.1.1"
                 , "bifunctors ^>= 5.5.14"
                 , "bimap ^>= 0.5.0"
                 , "binary-orphans ^>= 1.0.3"
                 , "bitvec ^>= 1.1.3.0"
                 , "blaze-builder ^>= 0.4.2.2"
                 , "blaze-html ^>= 0.9.1.2"
                 , "blaze-markup ^>= 0.8.2.8"
                 , "bloomfilter ^>= 2.0.1.0"
                 , "boring ^>= 0.2"
                 , "brick ^>= 1.6"
                 , "bsb-http-chunked ^>= 0.0.0.4"
                 , "byteable ^>= 0.1.1"
                 , "byteorder ^>= 1.0.4"
                 , "cabal-doctest ^>= 1.0.9"
                 , "cabal-install ^>= 3.8.1.0"
                 , "cabal-install-solver ^>= 3.8.1.0"
                 , "cabal-plan ^>= 0.7.2.3"
                 , "cabal2spec ^>= 2.6.4"
                 , "cairo ^>= 0.13.8.2"
                 , "call-stack ^>= 0.4.0"
                 , "case-insensitive ^>= 1.2.1.0"
                 , "cassava ^>= 0.5.3.0"
                 , "cassava-megaparsec ^>= 2.0.4"
                 , "cereal ^>= 0.5.8.3"
                 , "cipher-aes ^>= 0.2.11"
                 , "citeproc ^>= 0.8.1"
                 , "clientsession ^>= 0.9.1.2"
                 , "clock ^>= 0.8.3"
                 , "cmdargs ^>= 0.10.21"
                 , "code-page ^>= 0.2.1"
                 , "colour ^>= 2.3.6"
                 , "commonmark ^>= 0.2.2"
                 , "commonmark-extensions ^>= 0.2.3.3"
                 , "commonmark-pandoc ^>= 0.2.1.3"
                 , "comonad ^>= 5.0.8"
                 , "concurrent-output ^>= 1.10.17"
                 , "conduit ^>= 1.3.4.3"
                 , "conduit-extra ^>= 1.3.6"
                 , "config-ini ^>= 0.2.5.0"
                 , "connection ^>= 0.3.1"
                 , "constraints ^>= 0.13.4"
                 , "contravariant ^>= 1.5.5"
                 , "control-monad-free ^>= 0.6.2"
                 , "cookie ^>= 0.4.6"
                 , "cpphs ^>= 1.20.9.1"
                 , "cprng-aes ^>= 0.6.1"
                 , "criterion ^>= 1.6.0.0"
                 , "criterion-measurement ^>= 0.2.0.0"
                 , "crypto-api ^>= 0.13.3"
                 , "crypto-cipher-types ^>= 0.0.9"
                 , "crypto-random ^>= 0.0.9"
                 , "cryptohash-md5 ^>= 0.11.101.0"
                 , "cryptohash-sha1 ^>= 0.11.101.0"
                 , "cryptohash-sha256 ^>= 0.11.102.1"
                 , "cryptonite ^>= 0.30"
                 , "cryptonite-conduit ^>= 0.2.2"
                 , "css-text ^>= 0.1.3.0"
                 , "csv ^>= 0.1.2"
                 , "data-clist ^>= 0.2"
                 , "data-default ^>= 0.7.1.1"
                 , "data-default-class ^>= 0.1.2.0"
                 , "data-default-instances-containers ^>= 0.0.1"
                 , "data-default-instances-dlist ^>= 0.0.1"
                 , "data-default-instances-old-locale ^>= 0.0.1"
                 , "data-fix ^>= 0.3.2"
                 , "DAV ^>= 1.3.4"
                 , "dbus ^>= 1.2.27"
                 , "dec ^>= 0.0.5"
                 , "Decimal ^>= 0.5.2"
                 , "dense-linear-algebra ^>= 0.1.0.0"
                 , "deriving-aeson ^>= 0.2.9"
                 , "Diff ^>= 0.4.1"
                 , "digest ^>= 0.0.1.5"
                 , "disk-free-space ^>= 0.1.0.1"
                 , "distributive ^>= 0.6.2.1"
                 , "dlist ^>= 1.0"
                 , "doclayout ^>= 0.4"
                 , "doctemplates ^>= 0.11"
                 , "easy-file ^>= 0.2.2"
                 , "echo ^>= 0.1.4"
                 , "ed25519 ^>= 0.0.5.0"
                 , "edit-distance ^>= 0.2.2.1"
                 , "email-validate ^>= 2.3.2.18"
                 , "emojis ^>= 0.1.2"
                 , "entropy ^>= 0.4.1.10"
                 , "extensible-exceptions ^>= 0.1.1.4"
                 , "extra ^>= 1.7.12"
                 , "fast-logger ^>= 3.1.1"
                 , "fdo-notify ^>= 0.3.1"
                 , "feed ^>= 1.3.2.1"
                 , "fgl >=5.7.0 && <5.8.1.0"    -- TODO: ShellCheck-9.0 doesn't accept 8.5.1.x even though it's a minor update.
                 , "file-embed ^>= 0.0.15.0"
                 , "filepath-bytestring ^>= 1.4.2.1.12"
                 , "filepattern ^>= 0.1.3"
                 , "free ^>= 5.1.10"
                 , "fsnotify ^>= 0.4.1.0"
                 , "generically ^>= 0.1"
                 , "ghc-lib-parser ^>= 9.4.4.20221225"
                 , "ghc-lib-parser-ex ^>= 9.4.0.0"
                 , "git-annex ^>= 10.20230329"
                 , "githash ^>= 0.1.6.3"
                 , "glib ^>= 0.13.8.2"
                 , "Glob ^>= 0.10.2"
                 , "gridtables ^>= 0.1.0.0"
                 , "gtk2hs-buildtools ^>= 0.13.8.3"
                 , "hackage-security ^>= 0.6.2.3"
                 , "haddock-library ^>= 1.11.0"
                 , "happy ^>= 1.20.0"
                 , "hashable ^>= 1.4.2.0"
                 , "hashtables ^>= 1.3.1"
                 , "haskell-lexer ^>= 1.1.1"
                 , "hinotify ^>= 0.4.1"
                 , "hjsmin ^>= 0.2.1"
                 , "hledger ^>= 1.28"
                 , "hledger-interest ^>= 1.6.5"
                 , "hledger-lib ^>= 1.28"
                 , "hledger-ui ^>= 1.28"
                 , "hlint ^>= 3.5"
                 , "hourglass ^>= 0.2.12"
                 , "hsc2hs ^>= 0.68.8"
                 , "hscolour ^>= 1.24.4"
                 , "hslua ^>= 2.2.1"
                 , "hslua-aeson ^>= 2.2.1"
                 , "hslua-classes ^>= 2.2.0"
                 , "hslua-cli ^>= 1.2.0"
                 , "hslua-core ^>= 2.2.1"
                 , "hslua-list ^>= 1.1.0.1"
                 , "hslua-marshalling ^>= 2.2.1"
                 , "hslua-module-doclayout ^>= 1.0.4"
                 , "hslua-module-path ^>= 1.0.3"
                 , "hslua-module-system ^>= 1.0.2"
                 , "hslua-module-text ^>= 1.0.3.1"
                 , "hslua-module-version ^>= 1.0.3"
                 , "hslua-module-zip ^>= 1.0.0"
                 , "hslua-objectorientation ^>= 2.2.1"
                 , "hslua-packaging ^>= 2.2.1"
                 , "html ^>= 1.0.1.2"
                 , "HTTP ^>= 4000.4.1"
                 , "http-api-data >=0.4.1 && <0.5.1"    -- TODO: constraints from servant-0.19.1
                 , "http-client ^>= 0.7.13.1"
                 , "http-client-restricted ^>= 0.0.5"
                 , "http-client-tls ^>= 0.3.6.1"
                 , "http-conduit ^>= 2.3.8"
                 , "http-date ^>= 0.0.11"
                 , "http-media ^>= 0.8.0.0"
                 , "http-types ^>= 0.12.3"
                 , "http2 ^>= 3.0.3"
                 , "HUnit ^>= 1.6.2.0"
                 , "IfElse ^>= 0.85"
                 , "indexed-profunctors ^>= 0.1.1"
                 , "indexed-traversable ^>= 0.1.2"
                 , "indexed-traversable-instances ^>= 0.1.1.1"
                 , "integer-logarithms ^>= 1.0.3.1"
                 , "invariant ^>= 0.6"
                 , "iproute ^>= 1.7.12"
                 , "ipynb ^>= 0.2"
                 , "iwlib ^>= 0.1.2"
                 , "jira-wiki-markup ^>= 1.5.0"
                 , "js-chart ^>= 2.9.4.1"
                 , "JuicyPixels ^>= 3.3.8"
                 , "kan-extensions ^>= 5.2.5"
                 , "language-javascript ^>= 0.7.1.0"
                 , "lens ^>= 5.2"
                 , "libmpd ^>= 0.10.0.0"
                 , "libyaml ^>= 0.1.2"
                 , "lift-type ^>= 0.1.1.1"
                 , "lifted-base ^>= 0.2.3.12"
                 , "lpeg ^>= 1.0.3"
                 , "lua ^>= 2.2.1"
                 , "lucid ^>= 2.11.1"
                 , "lukko ^>= 0.1.1.3"
                 , "magic ^>= 1.1"
                 , "math-functions ^>= 0.3.4.2"
                 , "megaparsec ^>= 9.3.0"
                 , "memory ^>= 0.18.0"
                 , "microlens ^>= 0.4.13.1"
                 , "microlens-ghc ^>= 0.4.14.1"
                 , "microlens-mtl ^>= 0.2.0.3"
                 , "microlens-platform ^>= 0.4.3.3"
                 , "microlens-th ^>= 0.4.3.11"
                 , "microstache ^>= 1.0.2.3"
                 , "mime-types ^>= 0.1.1.0"
                 , "mmorph ^>= 1.2.0"
                 , "monad-control ^>= 1.0.3.1"
                 , "monad-logger ^>= 0.3.39"
                 , "monad-loops ^>= 0.4.3"
                 , "mono-traversable ^>= 1.0.15.3"
                 , "mountpoints ^>= 1.0.2"
                 , "mwc-random ^>= 0.15.0.2"
                 , "netlink ^>= 1.1.1.0"
                 , "network ^>= 3.1.2.7"
                 , "network-bsd ^>= 2.8.1.0"
                 , "network-byte-order ^>= 0.1.6"
                 , "network-info ^>= 0.2.1"
                 , "network-multicast ^>= 0.3.2"
                 , "network-uri ^>= 2.6.4.2"
                 , "old-locale ^>= 1.0.0.7"
                 , "old-time ^>= 1.1.0.3"
                 , "OneTuple ^>= 0.3.1"
                 , "Only ^>= 0.1"
                 , "optics-core ^>= 0.4.1"
                 , "optparse-applicative ^>= 0.17.0.0"
                 , "pandoc ^>= 3.0.1"
                 , "pandoc-cli ^>= 0.1"
                 , "pandoc-lua-engine == 0.1"
                 , "pandoc-lua-marshal ^>= 0.2.0"
                 , "pandoc-server ^>= 0.1"
                 , "pandoc-types ^>= 1.23"
                 , "pango ^>= 0.13.8.2"
                 , "parallel ^>= 3.2.2.0"
                 , "parsec-numbers ^>= 0.1.0"
                 , "parser-combinators ^>= 1.3.0"
                 , "path-pieces ^>= 0.2.1"
                 , "pem ^>= 0.2.4"
                 , "persistent ^>= 2.14.4.4"
                 , "persistent-sqlite ^>= 2.13.1.1"
                 , "polyparse ^>= 1.13"
                 , "postgresql-libpq ^>= 0.9.5.0"
                 , "postgresql-simple ^>= 0.6.5"
                 , "pretty-hex ^>= 1.1"
                 , "pretty-show ^>= 1.10"
                 , "pretty-simple ^>= 4.1.2.0"
                 , "prettyprinter ^>= 1.7.1"
                 , "prettyprinter-ansi-terminal ^>= 1.1.3"
                 , "primitive ^>= 0.7.4.0"
                 , "profunctors ^>= 5.6.2"
                 , "psqueues ^>= 0.2.7.3"
                 , "QuickCheck ^>= 2.14.2"
                 , "random ^>= 1.2.1.1"
                 , "recv ^>= 0.1.0"
                 , "refact ^>= 0.3.0.2"
                 , "reflection ^>= 2.1.6"
                 , "regex-base ^>= 0.94.0.2"
                 , "regex-compat ^>= 0.95.2.1"
                 , "regex-posix ^>= 0.96.0.1"
                 , "regex-tdfa ^>= 1.3.2"
                 , "resolv ^>= 0.1.2.0"
                 , "resource-pool ^>= 0.4.0.0"
                 , "resourcet ^>= 1.2.6"
                 , "safe ^>= 0.3.19"
                 , "safe-exceptions ^>= 0.1.7.3"
                 , "SafeSemaphore ^>= 0.10.1"
                 , "sandi ^>= 0.5"
                 , "scientific ^>= 0.3.7.0"
                 , "securemem ^>= 0.1.10"
                 , "semialign ^>= 1.2.0.1"
                 , "semigroupoids ^>= 5.3.7"
                 , "semigroups ^>= 0.20"
                 , "servant ^>= 0.19.1"
                 , "servant-server ^>= 0.19.2"
                 , "setenv ^>= 0.1.1.3"
                 , "setlocale ^>= 1.0.0.10"
                 , "SHA ^>= 1.6.4.4"
                 , "shakespeare ^>= 2.0.30"
                 , "ShellCheck ^>= 0.9.0"
                 , "silently ^>= 1.2.5.3"
                 , "simple-sendfile ^>= 0.2.30"
                 , "singleton-bool >=0.1.4 && <0.1.7"   -- TODO: constraints from servant-0.19.1
                 , "skein ^>= 1.0.9.4"
                 , "skylighting ^>= 0.13.2"
                 , "skylighting-core ^>= 0.13.2"
                 , "skylighting-format-ansi ^>= 0.1"
                 , "skylighting-format-blaze-html ^>= 0.1.1"
                 , "skylighting-format-context ^>= 0.1.0.1"
                 , "skylighting-format-latex ^>= 0.1"
                 , "socks ^>= 0.6.1"
                 , "some ^>= 1.0.4.1"
                 , "sop-core ^>= 0.5.0.2"
                 , "split ^>= 0.2.3.5"
                 , "splitmix ^>= 0.1.0.4"
                 , "StateVar ^>= 1.2.2"
                 , "statistics ^>= 0.16.1.2"
                 , "stm-chans ^>= 3.0.0.6"
                 , "streaming-commons ^>= 0.2.2.5"
                 , "strict ^>= 0.4.0.1"
                 , "string-conversions ^>= 0.4.0.1"
                 , "syb ^>= 0.7.2.2"
                 , "tabular ^>= 0.2.2.8"
                 , "tagged ^>= 0.8.6.1"
                 , "tagsoup ^>= 0.14.8"
                 , "tar ^>= 0.5.1.1"
                 , "tasty ^>= 1.4.3"
                 , "tasty-hunit ^>= 0.10.0.3"
                 , "tasty-quickcheck ^>= 0.10.2"
                 , "tasty-rerun ^>= 1.1.18"
                 , "temporary ^>= 1.3"
                 , "terminal-size ^>= 0.3.3"
                 , "texmath ^>= 0.12.6"
                 , "text-conversions ^>= 0.3.1.1"
                 , "text-short ^>= 0.1.5"
                 , "text-zipper ^>= 0.12"
                 , "th-abstraction ^>= 0.4.5.0"
                 , "th-compat ^>= 0.1.4"
                 , "th-lift ^>= 0.8.2"
                 , "th-lift-instances ^>= 0.1.20"
                 , "these ^>= 1.1.1.1"
                 , "time-compat ^>= 1.9.6.1"
                 , "time-locale-compat ^>= 0.1.1.5"
                 , "time-manager ^>= 0.0.0"
                 , "timeit ^>= 2.0"
                 , "timezone-olson ^>= 0.2.1"
                 , "timezone-series ^>= 0.1.13"
                 , "tls ^>= 1.6.0"
                 , "tls-session-manager ^>= 0.0.4"
                 , "topograph ^>= 1.0.0.2"
                 , "torrent ^>= 10000.1.1"
                 , "transformers-base ^>= 0.4.6"
                 , "transformers-compat ^>= 0.7.2"
                 , "type-equality ^>= 1"
                 , "typed-process ^>= 0.2.10.1"
                 , "uglymemo ^>= 0.1.0.1"
                 , "unicode-collation ^>= 0.1.3.3"
                 , "unicode-data ^>= 0.4.0.1"
                 , "unicode-transforms ^>= 0.4.0.1"
                 , "uniplate ^>= 1.6.13"
                 , "unix-compat ^>= 0.6"
                 , "unix-time ^>= 0.4.8"
                 , "unliftio ^>= 0.2.23.0"
                 , "unliftio-core ^>= 0.2.0.1"
                 , "unordered-containers ^>= 0.2.19.1"
                 , "utf8-string ^>= 1.0.2"
                 , "utility-ht ^>= 0.0.16"
                 , "uuid ^>= 1.3.15"
                 , "uuid-types ^>= 1.0.5"
                 , "vault ^>= 0.3.1.5"
                 , "vector ^>= 0.13.0.0"
                 , "vector-algorithms ^>= 0.9.0.1"
                 , "vector-binary-instances ^>= 0.2.5.2"
                 , "vector-stream ^>= 0.1.0.0"
                 , "vector-th-unbox ^>= 0.2.2"
                 , "void ^>= 0.7.3"
                 , "vty ^>= 5.38"
                 , "wai ^>= 3.2.3"
                 , "wai-app-static ^>= 3.1.7.4"
                 , "wai-cors ^>= 0.2.7"
                 , "wai-extra ^>= 3.1.13.0"
                 , "wai-logger ^>= 2.4.0"
                 , "warp ^>= 3.3.23"
                 , "warp-tls ^>= 3.3.4"
                 , "witherable ^>= 0.4.2"
                 , "wizards ^>= 1.0.3"
                 , "word-wrap ^>= 0.5"
                 , "word8 ^>= 0.1.3"
                 , "X11 ^>= 1.10.3"
                 , "X11-xft ^>= 0.3.4"
                 , "x509 ^>= 1.7.7"
                 , "x509-store ^>= 1.6.9"
                 , "x509-system ^>= 1.6.7"
                 , "x509-validation ^>= 1.6.12"
                 , "xml ^>= 1.3.14"
                 , "xml-conduit ^>= 1.9.1.1"
                 , "xml-hamlet ^>= 0.5.0.2"
                 , "xml-types ^>= 0.3.8"
                 , "xmobar ^>= 0.46"
                 , "xmonad ^>= 0.17.1"
                 , "xmonad-contrib ^>= 0.17.1"
                 , "xss-sanitize ^>= 0.3.7.1"
                 , "yaml ^>= 0.11.8.0"
                 , "yesod ^>= 1.6.2.1"
                 , "yesod-core ^>= 1.6.24.0"
                 , "yesod-form ^>= 1.7.3"
                 , "yesod-persistent ^>= 1.6.0.8"
                 , "yesod-static ^>= 1.6.1.0"
                 , "zip-archive ^>= 0.4.2.2"
                 , "zlib ^>= 0.6.3.0"
                 , "hadolint ^>= 2.12.0"
                 , "HsYAML"
                 , "colourista"
                 , "foldl"
                 , "gitrev >=1.3.1"
                 , "ilist"
                 , "language-docker (>=11.0.0 && <12) && >=11.0.0 && <12"
                 , "semver"
                 , "spdx"
                 , "timerep >=2.0"
                 , "monoid-subclasses >=0.4.1"
                 , "commutative-semigroups >=0.1 && <0.2"
                 , "primes >=0.2 && <0.3"
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
ghcCorePackages = [ "array-0.5.4.0"
                  , "base-4.17.0.0"
                  , "binary-0.8.9.1"
                  , "bytestring-0.11.3.1"
                  , "Cabal-3.8.1.0"
                  , "Cabal-syntax-3.8.1.0"
                  , "containers-0.6.6"
                  , "deepseq-1.4.8.0"
                  , "directory-1.3.7.1"
                  , "exceptions-0.10.5"
                  , "filepath-1.4.2.2"
                  , "ghc-9.4.4"
                  , "ghc-bignum-1.3"
                  , "ghc-boot-9.4.4"
                  , "ghc-boot-th-9.4.4"
                  , "ghc-compact-0.1.0.0"
                  , "ghc-heap-9.4.4"
                  , "ghc-prim-0.9.0"
                  , "ghci-9.4.4"
                  , "haskeline-0.8.2"
                  , "hpc-0.6.1.0"
                  , "integer-gmp-1.1"
                  , "libiserv-9.4.4"
                  , "mtl-2.2.2"
                  , "parsec-3.1.15.0"
                  , "pretty-1.1.3.6"
                  , "process-1.6.16.0"
                  , "rts-1.0.2"
                  , "stm-2.5.1.0"
                  , "system-cxx-std-lib-1.0"
                  , "template-haskell-2.19.0.0"
                  , "terminfo-0.4.1.5"
                  , "text-2.0.1"
                  , "time-1.12.2"
                  , "transformers-0.5.6.2"
                  , "unix-2.7.3"
                  , "xhtml-3000.2.2.1"

                  -- This is a bad hack. We have some Hackage packages that are
                  -- empty and that we don't want to package, but still opther
                  -- libraries depend on them being available. So we list them
                  -- as core packages to make the Cabal resolver happy even
                  -- though we actually don't use them at build time.
                  , "bytestring-builder-0.10.8.2.0"     -- now part of bytestring
                  , "persistent-template-2.12.0.0"      -- now part of persistent
                  ]

checkConsistency :: MonadFail m => PackageSetConfig -> m PackageSetConfig
checkConsistency pset@PackageSetConfig {..} = do
  let corePackagesInPackageSet = Map.keysSet packageSet `Set.intersection` Map.keysSet corePackages
  unless (Set.null corePackagesInPackageSet) $
    fail ("core packages listed in package set: " <> List.intercalate ", " (unPackageName <$> Set.toList corePackagesInPackageSet))
  pure pset
