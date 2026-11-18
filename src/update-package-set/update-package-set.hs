{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import Config.Compiler
import Config.Flags
import MyCabal
import Types

-- import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map.Strict ( toList )
import qualified Data.Set as Set
import Development.Shake.Command ( CmdOption(AddEnv), Stdout(..), cmd )
import Data.List ( intercalate )
import qualified Nix.Paths as Nix
-- import Distribution.Pretty
-- import Text.PrettyPrint ( pPrint, text )
import Text.PrettyPrint.HughesPJClass hiding ( (<>) )

-- | Generate a @cabal install --dry-run@ command that resolves the given target
-- packages under the constraints of our package set.
--
-- TODO: Honor deprecated packages in the Hackage tarball.

updatePackageSet :: FilePath -> Set.Set PackageName -> [String]
updatePackageSet ghc targets = ["cabal", "--store=/var/empty", "install", "--ignore-project", "--lib", "--dry-run", "--with-compiler=" <> ghc, "--minimize-conflict-set", "--upgrade-dependencies"]
                           ++ installed ++ lowerBounds ++ constraints ++ flags ++ allowNewer ++ pkgs
  where
    pkgs = display <$> Set.toList targets
    constraints = [ constraint (display (PackageVersionConstraint pn vr))
                  | (pn, vr) <- toList targetPackages
                  ]
    lowerBounds = [ constraint (unPackageName pn <> " >= " <> display v)
                  | (pn, v) <- toList currentPkgSet
                  ]
    installed = [ constraint (unPackageName pn <> " == " <> display v)
                | (pn, v) <- toList ghcCorePackages
                ]
    flags = [ constraint (unwords [pn, flags']) | (pn, flags') <- flagList ]
    allowNewer = [ "--allow-newer=" <> unPackageName x <> ":" <> unPackageName y
                 | (x, y) <- jailbreaks
                 ]
    constraint c = "--constraint=any." <> c

-- | Resolve 'targetPackages' and print the resulting package set.
--
-- A plan computed by @cabal install@ contains only the libraries of all
-- dependencies, but we build every package completely, including executables
-- and test suites. So we re-run the solver with all packages of the previous
-- plan as targets until the plan no longer changes.

main :: IO ()
main = do
  ghc <- compiler
  pkgConfig <- pkgConfigPath
  let solve targets = do Stdout out <- cmd (AddEnv "PKG_CONFIG_PATH" pkgConfig) (updatePackageSet ghc targets)
                         return out
  plan <- resolve solve (Map.keysSet targetPackages) Map.empty
  let cset = vcat $ punctuate comma [ doubleQuotes (pPrint pkg <> text " == " <> pPrint v) | (pkg,v) <- Map.toList plan
                                                                                           , pkg `Map.notMember` ghcCorePackages
                                    ]
  putStrLn $ display $ lbrack $$ nest 4 cset $$ rbrack

-- | Iterate the solver until the plan is stable.

resolve :: (Set.Set PackageName -> IO String) -> Set.Set PackageName -> PackageSet -> IO PackageSet
resolve solve targets previous = do
  plan <- parseInstallPlan <$> solve targets
  if plan == previous
     then return plan
     else resolve solve (Map.keysSet targetPackages `Set.union` Map.keysSet plan) plan

-- | Extract the package set from the install plan printed by @cabal install
-- --dry-run@. The relevant lines have the form
--
-- >  - aws-0.25.3 (lib) (requires download & build)
--
-- A package that contributes several components (e.g. a library and an
-- executable) appears more than once, but all entries share the same version.
-- Any line that does not match this format is ignored.

parseInstallPlan :: String -> PackageSet
parseInstallPlan out = Map.fromList
  [ (pkgName pid, pkgVersion pid)
  | l <- lines out
  , ("-" : pidStr : _) <- [words l]
  , Just pid <- [simpleParse pidStr]
  ]

-- | Instantiate the compiler from nixpkgs and return the path of its @ghc@
-- binary.

compiler :: IO FilePath
compiler = do
  ghc : _ <- nixBuild "(import <nixpkgs> {}).haskell.compiler.ghc914"
  return (ghc <> "/bin/ghc")

-- | System libraries (as nixpkgs attribute names) that packages in our set
-- look up via pkg-config, depending on the flags we choose.

pkgConfigDependencies :: [String]
pkgConfigDependencies = [ "libyaml", "sqlite" ]

-- | Instantiate all 'pkgConfigDependencies' from nixpkgs and return a search
-- path for pkg-config that covers their @.pc@ files.

pkgConfigPath :: IO String
pkgConfigPath = do
  paths <- nixBuild ("with import <nixpkgs> {}; map lib.getDev [ " <> unwords pkgConfigDependencies <> " ]")
  return $ intercalate ":" [ p <> d | p <- paths, d <- ["/lib/pkgconfig", "/share/pkgconfig"] ]

-- | Build a nix expression and return the store paths of its outputs.

nixBuild :: String -> IO [FilePath]
nixBuild expr = do
  Stdout out <- cmd Nix.nixBuild (["--no-out-link", "-E", expr] :: [String])
  return (lines out)

jailbreaks :: [(PackageName, PackageName)]
jailbreaks = [ ("serialise", "base"), ("serialise", "containers"), ("serialise", "time")
             , ("cborg", "base"), ("cborg", "containers")
             , ("persistent", "template-haskell")
             , ("git-lfs","containers")
             , ("feed","base"), ("feed","base-compat"), ("feed","time")
             , ("botan-low","base")
             , ("botan-bindings","base")
             ]

targetPackages :: ConstraintSet
targetPackages = [ "git-annex" ]

-- | The packages and versions of all generated builds that currently exist
-- in @_build/ghc-9.14.x@. The manually maintained builds (@ghc@,
-- @ghc-bootstrap@, @ghc-bootstrap-helpers@, @ghc-rpm-macros@, and @hadrian@)
-- are not included.

currentPkgSet :: PackageSet
currentPkgSet = [ "adjunctions-4.4.4"
                , "aeson-2.2.5.1"
                , "aeson-pretty-0.8.11"
                , "alex-3.5.4.2"
                , "alsa-core-0.5.0.1"
                , "alsa-mixer-0.3.0.1"
                , "ansi-terminal-1.1.5"
                , "ansi-terminal-types-1.1.3"
                , "appar-0.1.8"
                , "asciidoc-0.1.1.1"
                , "asn1-encoding-0.9.6"
                , "asn1-parse-0.9.5"
                , "asn1-types-0.3.4"
                , "assoc-1.1.1"
                , "async-2.2.6"
                , "atomic-counter-0.1.2.4"
                , "attoparsec-0.14.4"
                , "attoparsec-aeson-2.2.2.0"
                , "authenticate-oauth-1.7"
                , "auto-update-0.2.8"
                , "aws-0.24.4"
                , "base-compat-0.15.0"
                , "base-orphans-0.9.4"
                , "base-unicode-symbols-0.2.4.2"
                , "base16-bytestring-1.0.2.0"
                , "base64-bytestring-1.2.1.0"
                , "basement-0.0.16"
                , "bencode-0.6.1.1"
                , "bifunctors-5.6.3"
                , "bimap-0.5.0"
                , "bitvec-1.1.6.0"
                , "blake3-0.3.1"
                , "blaze-builder-0.4.4.1"
                , "blaze-html-0.9.2.0"
                , "blaze-markup-0.8.3.0"
                , "bloomfilter-2.0.1.3"
                , "boring-0.2.2.1"
                , "botan-bindings-0.3.0.0"
                , "botan-low-0.2.0.1"
                , "brick-3.0"
                , "bsb-http-chunked-0.0.0.4"
                , "byteable-0.1.1"
                , "byteorder-1.0.4"
                , "c2hs-0.28.8"
                , "cabal-doctest-1.0.13"
                , "cabal-install-3.14.1.1"
                , "cabal-install-solver-3.14.1.0"
                , "cabal-plan-0.7.7.0"
                , "cabal2spec-2.8.2"
                , "cairo-0.13.12.0"
                , "call-stack-0.4.0"
                , "case-insensitive-1.2.2.0"
                , "cassava-0.5.5.0"
                , "cassava-megaparsec-2.1.1"
                , "cborg-0.2.10.0"
                , "cereal-0.5.8.3"
                , "character-ps-0.1"
                , "citeproc-0.12"
                , "clientsession-0.9.3.0"
                , "clock-0.8.4"
                , "cmdargs-0.10.22"
                , "colour-2.3.7"
                , "commonmark-0.2.6.1"
                , "commonmark-extensions-0.2.8"
                , "commonmark-pandoc-0.2.3"
                , "comonad-5.0.10"
                , "concurrent-output-1.10.21"
                , "conduit-1.3.6.1"
                , "conduit-extra-1.3.8"
                , "config-ini-0.2.7.0"
                , "constraints-0.14.4"
                , "contravariant-1.5.6"
                , "control-monad-free-0.6.2"
                , "cookie-0.5.1"
                , "cpphs-1.20.10"
                , "crypto-api-0.13.3"
                , "crypto-pubkey-types-0.4.3"
                , "crypto-token-0.1.2"
                , "cryptohash-md5-0.11.101.0"
                , "cryptohash-sha1-0.11.101.0"
                , "cryptohash-sha256-0.11.102.1"
                , "crypton-1.0.6"
                , "crypton-conduit-0.3.1"
                , "crypton-connection-0.4.6"
                , "crypton-socks-0.6.2"
                , "crypton-x509-1.7.7"
                , "crypton-x509-store-1.6.14"
                , "crypton-x509-system-1.6.8"
                , "crypton-x509-validation-1.6.14"
                , "cryptonite-0.30"
                , "cryptonite-conduit-0.2.2"
                , "css-text-0.1.3.0"
                , "csv-0.1.2"
                , "data-clist-0.2"
                , "data-default-0.8.0.2"
                , "data-default-class-0.2.0.0"
                , "data-fix-0.3.4"
                , "DAV-1.3.4"
                , "dbus-1.4.3"
                , "dec-0.0.6"
                , "Decimal-0.5.2"
                , "deriving-aeson-0.2.10"
                , "Diff-2.0.1"
                , "digest-0.0.2.1"
                , "directory-ospath-streaming-0.2.2"
                , "disk-free-space-0.1.0.1"
                , "distributive-0.6.3"
                , "djot-0.1.4.3"
                , "dlist-1.0"
                , "dns-4.2.1"
                , "doclayout-0.5.0.3"
                , "doctemplates-0.11.1"
                , "easy-file-0.2.6"
                , "ech-config-0.0.2"
                , "echo-0.1.4"
                , "ed25519-0.0.5.0"
                , "edit-distance-0.2.2.1"
                , "email-validate-2.3.2.21"
                , "emojis-0.1.6"
                , "encoding-0.10.3"
                , "entropy-0.4.1.11"
                , "esqueleto-3.6.0.3"
                , "extensible-exceptions-0.1.1.4"
                , "extra-1.8.1"
                , "fast-logger-3.2.9"
                , "fdo-notify-0.3.1"
                , "feed-1.3.2.1"
                , "fgl-5.8.3.1"
                , "file-embed-0.0.16.0"
                , "filepath-bytestring-1.5.2.0.5"
                , "filepattern-0.1.3"
                , "free-5.2"
                , "fsnotify-0.4.4.0"
                , "generically-0.1.1"
                , "generics-sop-0.5.1.4"
                , "ghc-hs-meta-0.1.5.0"
                , "ghc-lib-parser-9.14.1.20251220"
                , "ghc-lib-parser-ex-9.14.2.0"
                , "git-annex-10.20261113"
                , "git-lfs-1.2.5"
                , "githash-0.1.7.0"
                , "glib-0.13.12.0"
                , "Glob-0.10.2"
                , "gridtables-0.1.1.1"
                , "gtk2hs-buildtools-0.13.12.0"
                , "hackage-security-0.6.4.0"
                , "half-0.3.3"
                , "happy-2.2"
                , "happy-lib-2.2"
                , "hashable-1.5.1.0"
                , "hashtables-1.4.2"
                , "haskell-lexer-1.2.1"
                , "haskell-src-exts-1.23.1"
                , "haskell-src-meta-0.8.16"
                , "hinotify-0.4.2"
                , "hjsmin-0.2.1"
                , "hledger-1.52.4"
                , "hledger-interest-2.0.0"
                , "hledger-lib-1.52.4"
                , "hledger-ui-1.52.4"
                , "hlint-3.10"
                , "hourglass-0.2.12"
                , "hpke-0.0.0"
                , "hscolour-1.25"
                , "hslogger-1.3.2.0"
                , "hslua-2.4.0"
                , "hslua-aeson-2.3.2.1"
                , "hslua-classes-2.3.2"
                , "hslua-cli-1.4.4"
                , "hslua-core-2.3.2.1"
                , "hslua-list-1.1.4"
                , "hslua-marshalling-2.3.2"
                , "hslua-module-doclayout-1.2.1.1"
                , "hslua-module-path-1.1.1"
                , "hslua-module-system-1.2.3"
                , "hslua-module-text-1.1.1"
                , "hslua-module-version-1.1.1"
                , "hslua-module-zip-1.1.4"
                , "hslua-objectorientation-2.4.0"
                , "hslua-packaging-2.3.2"
                , "hslua-repl-0.1.2"
                , "hslua-typing-0.1.1"
                , "html-1.0.1.2"
                , "HTTP-4000.4.1"
                , "http-api-data-0.6.3"
                , "http-client-0.7.19"
                , "http-client-restricted-0.1.1"
                , "http-client-tls-0.3.6.4"
                , "http-conduit-2.3.9.1"
                , "http-date-0.0.13"
                , "http-media-0.8.1.1"
                , "http-semantics-0.3.0"
                , "http-types-0.12.6"
                , "http2-5.3.10"
                , "HUnit-1.6.2.0"
                , "IfElse-0.85"
                , "indexed-profunctors-0.1.1.1"
                , "indexed-traversable-0.1.5"
                , "indexed-traversable-instances-0.1.2.1"
                , "integer-conversion-0.1.1"
                , "integer-logarithms-1.0.5"
                , "invariant-0.6.5"
                , "iproute-1.7.15"
                , "ipynb-0.2"
                , "isocline-1.0.9"
                , "iwlib-0.1.2"
                , "jira-wiki-markup-1.5.1"
                , "JuicyPixels-3.3.10"
                , "kan-extensions-5.2.8"
                , "language-c-0.10.2"
                , "language-javascript-0.7.1.0"
                , "lens-5.3.6"
                , "libmagic-1.3.3"
                , "libmpd-0.10.0.1"
                , "libyaml-0.1.4"
                , "lift-type-0.1.2.0"
                , "lifted-base-0.2.3.12"
                , "lpeg-1.1.0.1"
                , "lua-2.3.4"
                , "lucid-2.11.20260427"
                , "lukko-0.1.2"
                , "magic-1.1.2"
                , "math-functions-0.3.4.4"
                , "megaparsec-9.7.1"
                , "memory-0.18.0"
                , "microlens-0.5.0.0"
                , "microlens-ghc-0.4.15.2"
                , "microlens-mtl-0.2.1.1"
                , "microlens-platform-0.4.4.2"
                , "microlens-th-0.4.3.18"
                , "mime-types-0.1.2.2"
                , "mmorph-1.2.2"
                , "modern-uri-0.3.6.1"
                , "monad-control-1.0.3.1"
                , "monad-logger-0.3.42"
                , "monad-loops-0.4.3"
                , "mono-traversable-1.0.21.0"
                , "mountpoints-1.0.2"
                , "mtl-compat-0.2.2"
                , "netlink-1.1.1.0"
                , "network-3.2.14.0"
                , "network-bsd-2.8.1.0"
                , "network-byte-order-0.1.9"
                , "network-control-0.1.7"
                , "network-info-0.2.1"
                , "network-multicast-0.3.2"
                , "network-uri-2.6.4.2"
                , "old-locale-1.0.0.7"
                , "old-time-1.1.1.0"
                , "OneTuple-0.4.3"
                , "Only-0.1"
                , "open-browser-0.4.0.0"
                , "optics-core-0.4.2"
                , "optparse-applicative-0.19.0.0"
                , "ordered-containers-0.2.5"
                , "pandoc-3.14"
                , "pandoc-cli-3.14"
                , "pandoc-lua-engine-0.5.4.3"
                , "pandoc-lua-marshal-0.3.3"
                , "pandoc-server-0.1.4.3"
                , "pandoc-types-1.23.1.2"
                , "pango-0.13.12.0"
                , "parallel-3.2.2.0"
                , "parsec-numbers-0.1.0"
                , "parser-combinators-1.3.1"
                , "path-pieces-0.2.1"
                , "pem-0.2.4"
                , "persistent-2.17.1.0"
                , "persistent-sqlite-2.13.3.1"
                , "persistent-template-2.12.0.0"
                , "polyparse-1.13.1"
                , "postgresql-libpq-0.11.0.0"
                , "postgresql-libpq-configure-0.11"
                , "postgresql-simple-0.7.0.1"
                , "pretty-hex-1.1"
                , "pretty-show-1.10"
                , "pretty-simple-4.1.4.0"
                , "prettyprinter-1.7.2"
                , "prettyprinter-ansi-terminal-1.1.4"
                , "primitive-0.9.1.0"
                , "profunctors-5.6.3"
                , "psqueues-0.2.8.3"
                , "QuickCheck-2.15.0.1"
                , "quote-quot-0.2.2.0"
                , "ram-0.22.1"
                , "random-1.3.1"
                , "raw-strings-qq-1.1"
                , "recv-0.1.3"
                , "refact-0.3.0.2"
                , "reflection-2.1.9"
                , "regex-base-0.94.0.3"
                , "regex-compat-0.95.2.2"
                , "regex-posix-0.96.0.2"
                , "regex-tdfa-1.3.2.7"
                , "replace-megaparsec-1.5.0.1"
                , "req-3.13.4"
                , "resolv-0.2.0.3"
                , "resource-pool-0.5.1.0"
                , "resourcet-1.3.0"
                , "retry-0.9.3.1"
                , "RSA-2.4.1"
                , "safe-0.3.21"
                , "safe-exceptions-0.1.7.4"
                , "SafeSemaphore-0.10.1"
                , "sandi-0.5"
                , "scientific-0.3.10.0"
                , "securemem-0.1.10"
                , "semialign-1.3.1.1"
                , "semigroupoids-6.0.2"
                , "semigroups-0.20.1"
                , "serialise-0.2.6.1"
                , "servant-0.20.3.0"
                , "servant-client-0.20.3.0"
                , "servant-client-core-0.20.3.0"
                , "servant-server-0.20.3.0"
                , "setenv-0.1.1.3"
                , "setlocale-1.0.0.10"
                , "SHA-1.6.4.4"
                , "shakespeare-2.1.7.1"
                , "ShellCheck-0.11.0"
                , "silently-1.2.5.4"
                , "simple-sendfile-0.2.33"
                , "singleton-bool-0.1.8"
                , "skein-1.0.9.4"
                , "skylighting-0.14.7"
                , "skylighting-core-0.14.7"
                , "skylighting-format-ansi-0.1"
                , "skylighting-format-blaze-html-0.1.2.2"
                , "skylighting-format-context-0.1.0.2"
                , "skylighting-format-latex-0.1"
                , "skylighting-format-typst-0.1"
                , "socks-0.6.1"
                , "some-1.0.6"
                , "sop-core-0.5.0.2"
                , "split-0.2.5.1"
                , "splitmix-0.1.3.2"
                , "StateVar-1.2.2"
                , "stm-chans-3.0.0.11"
                , "streaming-commons-0.2.3.1"
                , "strict-0.5.1"
                , "string-interpolate-1.0.0.0"
                , "syb-0.7.4"
                , "tabular-0.2.2.8"
                , "tagged-0.8.11"
                , "tagsoup-0.14.8"
                , "tar-0.6.4.0"
                , "tasty-1.5.5"
                , "tasty-hunit-0.10.2"
                , "tasty-quickcheck-0.11.1"
                , "tasty-rerun-1.1.20"
                , "temporary-1.3"
                , "terminal-size-0.3.4"
                , "texmath-0.13.3.5"
                , "text-ansi-0.3.0.1"
                , "text-builder-1.0.0.5"
                , "text-builder-core-0.1.1.3"
                , "text-builder-linear-0.1.4"
                , "text-conversions-0.3.1.1"
                , "text-icu-0.8.0.5"
                , "text-iso8601-0.1.1.2"
                , "text-short-0.1.6.1"
                , "text-zipper-0.13"
                , "th-abstraction-0.7.2.0"
                , "th-compat-0.1.7"
                , "th-expand-syns-0.4.12.0"
                , "th-lift-0.8.7"
                , "th-lift-instances-0.1.20"
                , "th-orphans-0.13.17"
                , "th-reify-many-0.1.10"
                , "these-1.2.1"
                , "time-compat-1.9.9"
                , "time-locale-compat-0.1.1.5"
                , "time-manager-0.2.4"
                , "timeit-2.0"
                , "timezone-olson-0.2.1"
                , "timezone-series-0.1.13"
                , "tls-2.1.14"
                , "tls-session-manager-0.0.9"
                , "toml-parser-2.0.2.0"
                , "topograph-1.0.1"
                , "torrent-10000.1.3"
                , "transformers-base-0.4.6.1"
                , "transformers-compat-0.7.2"
                , "typed-process-0.2.13.0"
                , "typst-0.8.1"
                , "typst-symbols-0.1.9.1"
                , "uglymemo-0.1.0.1"
                , "unbounded-delays-0.1.1.1"
                , "unicode-collation-0.1.3.7"
                , "unicode-data-0.6.0"
                , "unicode-transforms-0.4.0.1"
                , "uniplate-1.6.13"
                , "unix-compat-0.7.4.1"
                , "unix-time-0.4.17"
                , "unliftio-0.2.25.1"
                , "unliftio-core-0.2.1.0"
                , "unordered-containers-0.2.21"
                , "utf8-string-1.0.2"
                , "utility-ht-0.0.17.2"
                , "uuid-1.3.16.1"
                , "uuid-types-1.0.6.1"
                , "vault-0.3.2.0"
                , "vector-0.13.2.0"
                , "vector-algorithms-0.9.1.0"
                , "vector-stream-0.1.0.1"
                , "void-0.7.4"
                , "vty-6.7"
                , "vty-crossplatform-0.4.0.0"
                , "vty-unix-0.5.0.0"
                , "wai-3.2.5"
                , "wai-app-static-3.1.9.1"
                , "wai-cors-0.2.7"
                , "wai-extra-3.1.18"
                , "wai-logger-2.5.1"
                , "warp-3.4.9"
                , "warp-tls-3.4.18"
                , "witherable-0.5"
                , "wizards-1.0.3"
                , "word-wrap-0.5"
                , "word8-0.1.4"
                , "X11-1.10.3"
                , "X11-xft-0.3.4"
                , "xml-1.3.14"
                , "xml-conduit-1.10.1.0"
                , "xml-hamlet-0.5.0.3"
                , "xml-types-0.3.8"
                , "xmobar-0.51.1"
                , "xmonad-0.18.1"
                , "xmonad-contrib-0.18.2"
                , "xss-sanitize-0.3.7.2"
                , "xxhash-ffi-0.3.1"
                , "yaml-0.11.11.2"
                , "yamlet-1.0.0.2"
                , "yamlet-aeson-1.0.0.1"
                , "yesod-1.6.2.3"
                , "yesod-core-1.6.29.1"
                , "yesod-form-1.7.11"
                , "yesod-persistent-1.6.0.9"
                , "yesod-static-1.6.1.3"
                , "zip-archive-0.4.3.2"
                , "zlib-0.7.1.1"
                ]
