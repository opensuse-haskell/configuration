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

-- targetPackages :: ConstraintSet
-- targetPackages   = [ "pandoc-cli"
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
constraintList = [ "aeson ^>= 2.1.2.1"
                 , "aeson-pretty ^>= 0.8.10"
                 , "alex ^>= 3.4.0.1"
                 , "ansi-terminal ^>= 1.0"
                 , "ansi-terminal-types ^>= 0.11.5"
                 , "appar ^>= 0.1.8"
                 , "asn1-encoding ^>= 0.9.6"
                 , "asn1-parse ^>= 0.9.5"
                 , "asn1-types ^>= 0.3.4"
                 , "assoc ^>= 1.1"
                 , "async ^>= 2.2.5"
                 , "attoparsec ^>= 0.14.4"
                 , "auto-update ^>= 0.1.6"
                 , "base-compat ^>= 0.13.1"
                 , "base-compat-batteries ^>= 0.13.1"
                 , "base-orphans ^>= 0.9.1"
                 , "base-unicode-symbols ^>= 0.2.4.2"
                 , "base16-bytestring ^>= 1.0.2.0"
                 , "base64 ^>= 0.4.2.4"
                 , "base64-bytestring ^>= 1.2.1.0"
                 , "basement ^>= 0.0.16"
                 , "bifunctors ^>= 5.6.1"
                 , "bitvec ^>= 1.1.5.0"
                 , "blaze-builder ^>= 0.4.2.3"
                 , "blaze-html ^>= 0.9.1.2"
                 , "blaze-markup ^>= 0.8.3.0"
                 , "boring ^>= 0.2.1"
                 , "bsb-http-chunked ^>= 0.0.0.4"
                 , "byteorder ^>= 1.0.4"
                 , "cabal-doctest ^>= 1.0.9"
                 , "call-stack ^>= 0.4.0"
                 , "case-insensitive ^>= 1.2.1.0"
                 , "cassava ^>= 0.5.3.0"
                 , "cereal ^>= 0.5.8.3"
                 , "citeproc ^>= 0.8.1"
                 , "colour ^>= 2.3.6"
                 , "commonmark ^>= 0.2.4"
                 , "commonmark-extensions ^>= 0.2.4"
                 , "commonmark-pandoc ^>= 0.2.1.3"
                 , "comonad ^>= 5.0.8"
                 , "conduit ^>= 1.3.5"
                 , "conduit-extra ^>= 1.3.6"
                 , "constraints ^>= 0.14"
                 , "contravariant ^>= 1.5.5"
                 , "cookie ^>= 0.4.6"
                 , "crypton ^>= 0.34"
                 , "crypton-connection ^>= 0.3.1"
                 , "crypton-x509 ^>= 1.7.6"
                 , "crypton-x509-store ^>= 1.6.9"
                 , "crypton-x509-system ^>= 1.6.7"
                 , "crypton-x509-validation ^>= 1.6.12"
                 , "data-array-byte ^>= 0.1.0.1"
                 , "data-default ^>= 0.7.1.1"
                 , "data-default-class ^>= 0.1.2.0"
                 , "data-default-instances-containers ^>= 0.0.1"
                 , "data-default-instances-dlist ^>= 0.0.1"
                 , "data-default-instances-old-locale ^>= 0.0.1"
                 , "data-fix ^>= 0.3.2"
                 , "dec ^>= 0.0.5"
                 , "digest ^>= 0.0.1.3"
                 , "digits ^>= 0.3.1"
                 , "distributive ^>= 0.6.2.1"
                 , "dlist ^>= 1.0"
                 , "doclayout ^>= 0.4.0.1"
                 , "doctemplates ^>= 0.11"
                 , "easy-file ^>= 0.2.5"
                 , "emojis ^>= 0.1.3"
                 , "fast-logger ^>= 3.2.2"
                 , "file-embed ^>= 0.0.15.0"
                 , "foldable1-classes-compat ^>= 0.1"
                 , "generically ^>= 0.1.1"
                 , "Glob ^>= 0.10.2"
                 , "gridtables ^>= 0.1.0.0"
                 , "haddock-library ^>= 1.11.0"
                 , "hashable ^>= 1.4.3.0"
                 , "haskell-lexer ^>= 1.1.1"
                 , "hourglass ^>= 0.2.12"
                 , "hslua ^>= 2.3.0"
                 , "hslua-aeson ^>= 2.3.0.1"
                 , "hslua-classes ^>= 2.3.0"
                 , "hslua-cli ^>= 1.4.1"
                 , "hslua-core ^>= 2.3.1"
                 , "hslua-list ^>= 1.1.1"
                 , "hslua-marshalling ^>= 2.3.0"
                 , "hslua-module-doclayout ^>= 1.1.0"
                 , "hslua-module-path ^>= 1.1.0"
                 , "hslua-module-system ^>= 1.1.0.1"
                 , "hslua-module-text ^>= 1.1.0.1"
                 , "hslua-module-version ^>= 1.1.0"
                 , "hslua-module-zip ^>= 1.1.0"
                 , "hslua-objectorientation ^>= 2.3.0"
                 , "hslua-packaging ^>= 2.3.0"
                 , "hslua-repl ^>= 0.1.1"
                 , "hslua-typing ^>= 0.1.0"
                 , "http-api-data ^>= 0.6"
                 , "http-client ^>= 0.7.15"
                 , "http-client-tls ^>= 0.3.6.3"
                 , "http-date ^>= 0.0.11"
                 , "http-media ^>= 0.8.1.1"
                 , "http-types ^>= 0.12.4"
                 , "http2 ^>= 5.0.0"
                 , "HUnit ^>= 1.6.2.0"
                 , "indexed-traversable ^>= 0.1.3"
                 , "indexed-traversable-instances ^>= 0.1.1.2"
                 , "integer-conversion ^>= 0.1.0.1"
                 , "integer-logarithms ^>= 1.0.3.1"
                 , "iproute ^>= 1.7.12"
                 , "ipynb ^>= 0.2"
                 , "isocline ^>= 1.0.9"
                 , "jira-wiki-markup ^>= 1.5.1"
                 , "JuicyPixels ^>= 3.3.8"
                 , "libyaml ^>= 0.1.2"
                 , "lpeg ^>= 1.0.4"
                 , "lua ^>= 2.3.1"
                 , "memory ^>= 0.18.0"
                 , "mime-types ^>= 0.1.2.0"
                 , "mmorph ^>= 1.2.0"
                 , "monad-control ^>= 1.0.3.1"
                 , "mono-traversable ^>= 1.0.15.3"
                 , "network ^>= 3.1.4.0"
                 , "network-byte-order ^>= 0.1.7"
                 , "network-control ^>= 0.0.2"
                 , "network-uri ^>= 2.6.4.2"
                 , "old-locale ^>= 1.0.0.7"
                 , "old-time ^>= 1.1.0.3"
                 , "OneTuple ^>= 0.4.1.1"
                 , "Only ^>= 0.1"
                 , "optparse-applicative ^>= 0.18.1.0"
                 , "ordered-containers ^>= 0.2.3"
                 , "pandoc ^>= 3.1.9"
                 , "pandoc-cli"
                 , "pandoc-lua-engine ^>= 0.2.1.2"
                 , "pandoc-lua-marshal ^>= 0.2.2"
                 , "pandoc-server ^>= 0.1.0.3"
                 , "pandoc-types ^>= 1.23.1"
                 , "pem ^>= 0.2.4"
                 , "pretty-show ^>= 1.10"
                 , "prettyprinter ^>= 1.7.1"
                 , "prettyprinter-ansi-terminal ^>= 1.1.3"
                 , "primitive ^>= 0.8.0.0"
                 , "psqueues ^>= 0.2.8.0"
                 , "QuickCheck ^>= 2.14.3"
                 , "random ^>= 1.2.1.1"
                 , "recv ^>= 0.1.0"
                 , "regex-base ^>= 0.94.0.2"
                 , "regex-tdfa ^>= 1.3.2.2"
                 , "resourcet ^>= 1.3.0"
                 , "safe ^>= 0.3.19"
                 , "safe-exceptions ^>= 0.1.7.4"
                 , "scientific ^>= 0.3.7.0"
                 , "semialign ^>= 1.3"
                 , "semigroupoids ^>= 6.0.0.1"
                 , "servant ^>= 0.20.1"
                 , "servant-server ^>= 0.20"
                 , "SHA ^>= 1.6.4.4"
                 , "simple-sendfile ^>= 0.2.32"
                 , "singleton-bool ^>= 0.1.7"
                 , "skylighting ^>= 0.14.1"
                 , "skylighting-core ^>= 0.14.1"
                 , "skylighting-format-ansi ^>= 0.1"
                 , "skylighting-format-blaze-html ^>= 0.1.1.1"
                 , "skylighting-format-context ^>= 0.1.0.2"
                 , "skylighting-format-latex ^>= 0.1"
                 , "socks ^>= 0.6.1"
                 , "some ^>= 1.0.6"
                 , "sop-core ^>= 0.5.0.2"
                 , "split ^>= 0.2.4"
                 , "splitmix ^>= 0.1.0.5"
                 , "StateVar ^>= 1.2.2"
                 , "streaming-commons ^>= 0.2.2.6"
                 , "strict ^>= 0.5"
                 , "string-conversions ^>= 0.4.0.1"
                 , "syb ^>= 0.7.2.4"
                 , "tagged ^>= 0.8.8"
                 , "tagsoup ^>= 0.14.8"
                 , "temporary ^>= 1.3"
                 , "texmath ^>= 0.12.8.4"
                 , "text-conversions ^>= 0.3.1.1"
                 , "text-iso8601 ^>= 0.1"
                 , "text-short ^>= 0.1.5"
                 , "th-abstraction ^>= 0.5.0.0"
                 , "th-compat ^>= 0.1.4"
                 , "th-lift ^>= 0.8.4"
                 , "th-lift-instances ^>= 0.1.20"
                 , "these ^>= 1.2"
                 , "time-compat ^>= 1.9.6.1"
                 , "time-manager ^>= 0.0.1"
                 , "tls ^>= 1.9.0"
                 , "toml-parser ^>= 1.3.1.0"
                 , "transformers-base ^>= 0.4.6"
                 , "transformers-compat ^>= 0.7.2"
                 , "type-equality ^>= 1"
                 , "typed-process ^>= 0.2.11.1"
                 , "typst ^>= 0.5"
                 , "typst-symbols == 0.1.5.*"
                 , "unicode-collation ^>= 0.1.3.5"
                 , "unicode-data ^>= 0.4.0.1"
                 , "unicode-transforms ^>= 0.4.0.1"
                 , "uniplate ^>= 1.6.13"
                 , "unix-compat ^>= 0.7.1"
                 , "unix-time ^>= 0.4.11"
                 , "unliftio ^>= 0.2.25.0"
                 , "unliftio-core ^>= 0.2.1.0"
                 , "unordered-containers ^>= 0.2.19.1"
                 , "utf8-string ^>= 1.0.2"
                 , "uuid-types ^>= 1.0.5.1"
                 , "vault ^>= 0.3.1.5"
                 , "vector ^>= 0.13.1.0"
                 , "vector-algorithms ^>= 0.9.0.1"
                 , "vector-stream ^>= 0.1.0.0"
                 , "wai ^>= 3.2.3"
                 , "wai-app-static ^>= 3.1.8"
                 , "wai-cors ^>= 0.2.7"
                 , "wai-extra ^>= 3.1.13.0"
                 , "wai-logger ^>= 2.4.0"
                 , "warp ^>= 3.3.31"
                 , "witherable ^>= 0.4.2"
                 , "word8 ^>= 0.1.3"
                 , "xml ^>= 1.3.14"
                 , "xml-conduit ^>= 1.9.1.3"
                 , "xml-types ^>= 0.3.8"
                 , "yaml ^>= 0.11.11.2"
                 , "zip-archive ^>= 0.4.3"
                 , "zlib ^>= 0.6.3.0"
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

    -- Enable LUA support.
  , ("pandoc-cli",                     "+lua +server")
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
