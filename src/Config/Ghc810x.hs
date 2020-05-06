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
targetPackages   = [ "alex >=3.2.5"
                   , "cabal-install ==3.2.*"
                   , "cabal2spec >=2.6"
                   , "distribution-opensuse >= 1.1.1"
                   , "happy >=1.19.12"
                   , "pandoc >=2.9.2.1"
                   , "pandoc-citeproc >=0.17"
                   , "ShellCheck >=0.7.1"
                -- , "stack >=1.7.1 && < 9.9.9"
                   , "xmobar >= 0.33"
                   , "xmonad >= 0.15"
                   , "xmonad-contrib >= 0.16"
                   , "SDL >=0.6.6.0"        -- Dmitriy Perlow <dap.darkness@gmail.com> needs the
                   , "SDL-image >=0.6.2.0"  -- SDL packages for games/raincat.
                   , "SDL-mixer >=0.6.3.0"
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

constraintList :: ConstraintSet
constraintList = [ "adjunctions ==4.4"
                 , "aeson ==1.4.7.1"
                 , "aeson-pretty ==0.8.8"
                 , "alex ==3.2.5"
                 , "ansi-terminal ==0.10.3"
                 , "ansi-wl-pprint ==0.6.9"
                 , "asn1-encoding ==0.9.6"
                 , "asn1-parse ==0.9.5"
                 , "asn1-types ==0.3.4"
                 , "async ==2.2.2"
                 , "attoparsec ==0.13.2.4"
                 , "base-compat ==0.11.1"
                 , "base-compat-batteries ==0.11.1"
                 , "base-noprelude ==4.13.0.0"
                 , "base-orphans ==0.8.2"
                 , "base-prelude ==1.3"
                 , "base16-bytestring ==0.1.1.6"
                 , "base64-bytestring ==1.0.0.3"
                 , "basement ==0.0.11"
                 , "bifunctors ==5.5.7"
                 , "bitarray ==0.0.1.1"
                 , "blaze-builder ==0.4.1.0"
                 , "blaze-html ==0.9.1.2"
                 , "blaze-markup ==0.8.2.5"
                 , "cabal-doctest ==1.0.8"
                 , "cabal-install ==3.2.0.0"
                 , "cabal2spec ==2.6"
                 , "call-stack ==0.2.0"
                 , "case-insensitive ==1.2.1.0"
                 , "cereal ==0.5.8.1"
                 , "clock ==0.8"
                 , "cmark-gfm ==0.2.1"
                 , "colour ==2.3.5"
                 , "comonad ==5.0.6"
                 , "conduit ==1.3.2"
                 , "conduit-extra ==1.3.5"
                 , "connection ==0.3.1"
                 , "contravariant ==1.5.2"
                 , "cookie ==0.4.5"
                 , "cryptohash-sha256 ==0.11.101.0"
                 , "cryptonite ==0.26"
                 , "data-default ==0.7.1.1"
                 , "data-default-class ==0.1.2.0"
                 , "data-default-instances-containers ==0.0.1"
                 , "data-default-instances-dlist ==0.0.1"
                 , "data-default-instances-old-locale ==0.0.1"
                 , "dbus ==1.2.14"
                 , "Diff ==0.4.0"
                 , "digest ==0.0.1.2"
                 , "distribution-opensuse ==1.1.1"
                 , "distributive ==0.6.2"
                 , "dlist ==0.8.0.8"
                 , "doclayout ==0.3"
                 , "doctemplates ==0.8.2"
                 , "echo ==0.1.3"
                 , "ed25519 ==0.0.*"
                 , "edit-distance ==0.2.2.1"
                 , "emojis ==0.1"
                 , "errors ==2.3.0"
                 , "extensible-exceptions ==0.1.1.4"
                 , "extra ==1.7.1"
                 , "fail ==4.9.0.0"
                 , "foldl ==1.4.6"
                 , "free ==5.1.3"
                 , "Glob ==0.10.0"
                 , "hackage-security ==0.6.0.1"
                 , "haddock-library ==1.9.0"
                 , "happy ==1.19.12"
                 , "hashable ==1.3.0.0"
                 , "hinotify ==0.4"
                 , "hostname ==1.0"
                 , "hourglass ==0.2.12"
                 , "hs-bibutils ==6.10.0.0"
                 , "hsemail ==2.2.0"
                 , "hslua ==1.0.3.2"
                 , "hslua-module-system ==0.2.1"
                 , "hslua-module-text ==0.2.1"
                 , "HsYAML ==0.2.1.0"
                 , "HsYAML-aeson ==0.2.0.0"
                 , "HTTP ==4000.3.14"
                 , "http-client ==0.6.4.1"
                 , "http-client-tls ==0.3.5.3"
                 , "http-conduit ==2.3.7.3"
                 , "http-types ==0.12.3"
                 , "hxt ==9.3.1.18"
                 , "hxt-charproperties ==9.4.0.0"
                 , "hxt-regex-xmlschema ==9.2.0.3"
                 , "hxt-unicode ==9.0.2.4"
                 , "integer-logarithms ==1.0.3"
                 , "invariant ==0.5.3"
                 , "ipynb ==0.1.0.1"
                 , "iwlib ==0.1.0"
                 , "jira-wiki-markup ==1.1.4"
                 , "JuicyPixels ==3.3.5"
                 , "kan-extensions ==5.2"
                 , "lens ==4.19.2"
                 , "libyaml ==0.1.2"
                 , "lukko ==0.1.1.2"
                 , "managed ==1.0.7"
                 , "math-functions ==0.3.3.0"
                 , "memory ==0.15.0"
                 , "mime-types ==0.1.0.9"
                 , "mono-traversable ==1.0.15.1"
                 , "mwc-random ==0.14.0.0"
                 , "network ==3.1.1.1"
                 , "network-uri ==2.6.*"
                 , "old-locale ==1.0.0.7"
                 , "old-time ==1.1.0.3"
                 , "optional-args ==1.0.2"
                 , "optparse-applicative ==0.15.1.0"
                 , "pandoc ==2.9.2.1"
                 , "pandoc-citeproc ==0.17"
                 , "pandoc-types ==1.20"
                 , "parallel ==3.2.2.0"
                 , "parsec-class ==1.0.0.0"
                 , "parsec-numbers ==0.1.0"
                 , "pem ==0.2.4"
                 , "primitive ==0.7.0.1"
                 , "profunctors ==5.5.2"
                 , "QuickCheck ==2.13.2"
                 , "random ==1.1"
                 , "reflection ==2.1.5"
                 , "regex-base ==0.94.0.0"
                 , "regex-compat ==0.95.2.0"
                 , "regex-pcre-builtin ==0.95.1.2.8.43"
                 , "regex-posix ==0.96.0.0"
                 , "regex-tdfa ==1.3.1.0"
                 , "resolv ==0.1.2.0"
                 , "resourcet ==1.2.4"
                 , "rfc5051 ==0.1.0.4"
                 , "safe ==0.3.18"
                 , "scientific ==0.3.6.2"
                 , "SDL ==0.6.7.0"
                 , "SDL-image ==0.6.2.0"
                 , "SDL-mixer ==0.6.3.0"
                 , "semigroupoids ==5.3.4"
                 , "semigroups ==0.19.1"
                 , "setenv ==0.1.1.3"
                 , "setlocale ==1.0.0.9"
                 , "SHA ==1.6.4.4"
                 , "ShellCheck ==0.7.1"
                 , "skylighting ==0.8.3.4"
                 , "skylighting-core ==0.8.3.4"
                 , "socks ==0.6.1"
                 , "split ==0.2.3.4"
                 , "splitmix ==0.0.4"
                 , "StateVar ==1.2"
                 , "streaming-commons ==0.2.1.2"
                 , "syb ==0.7.1"
                 , "system-fileio ==0.3.16.4"
                 , "system-filepath ==0.4.14"
                 , "tagged ==0.8.6"
                 , "tagsoup ==0.14.8"
                 , "tar ==0.5.1.1"
                 , "temporary ==1.3"
                 , "texmath ==0.12.0.2"
                 , "text-conversions ==0.3.0"
                 , "th-abstraction ==0.3.2.0"
                 , "th-lift ==0.8.1"
                 , "time-compat ==1.9.3"
                 , "timezone-olson ==0.1.9"
                 , "timezone-series ==0.1.9"
                 , "tls ==1.5.4"
                 , "transformers-base ==0.4.5.2"
                 , "transformers-compat ==0.6.5"
                 , "turtle ==1.5.19"
                 , "typed-process ==0.2.6.0"
                 , "unicode-transforms ==0.3.6"
                 , "unix-compat ==0.5.2"
                 , "unliftio-core ==0.2.0.1"
                 , "unordered-containers ==0.2.10.0"
                 , "utf8-string ==1.0.1.1"
                 , "uuid-types ==1.0.3"
                 , "vector ==0.12.1.2"
                 , "vector-algorithms ==0.8.0.3"
                 , "vector-builder ==0.3.8"
                 , "vector-th-unbox ==0.2.1.7"
                 , "void ==0.7.3"
                 , "X11 ==1.9.1"
                 , "X11-xft ==0.3.1"
                 , "x509 ==1.7.5"
                 , "x509-store ==1.6.7"
                 , "x509-system ==1.6.6"
                 , "x509-validation ==1.6.11"
                 , "xml ==1.3.14"
                 , "xml-conduit ==1.9.0.0"
                 , "xml-types ==0.3.6"
                 , "xmobar ==0.33"
                 , "xmonad ==0.15"
                 , "xmonad-contrib ==0.16"
                 , "yaml ==0.11.3.0"
                 , "zip-archive ==0.4.1"
                 , "zlib ==0.6.2.1"
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

    -- The command-line utility pulls in other dependencies.
  , ("aeson-pretty",                   "lib-only")
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
               , "binary ==0.8.8.0"
               , "bytestring ==0.10.10.0"
               , "Cabal ==3.2.0.0"
               , "containers ==0.6.2.1"
               , "deepseq ==1.4.4.0"
               , "directory ==1.3.6.0"
               , "exceptions ==0.10.4"
               , "filepath ==1.4.2.1"
               , "ghc ==8.10.1"
               , "ghc-boot ==8.10.1"
               , "ghc-boot-th ==8.10.1"
               , "ghc-compact ==0.1.0.0"
               , "ghc-heap ==8.10.1"
               , "ghc-prim ==0.6.1"
               , "ghci ==8.10.1"
               , "haskeline ==0.8.0.0"
               , "hpc ==0.6.1.0"
               , "hsc2hs ==0.68.7"
               , "integer-gmp ==1.0.3.0"
               , "libiserv ==8.10.1"
               , "mtl ==2.2.2"
               , "parsec ==3.1.14.0"
               , "pretty ==1.1.3.6"
               , "process ==1.6.8.2"
               , "rts ==1.0"
               , "stm ==2.5.0.0"
               , "template-haskell ==2.16.0.0"
               , "terminfo ==0.4.1.4"
               , "text ==1.2.3.2"
               , "time ==1.9.3"
               , "transformers ==0.5.6.2"
               , "unix ==2.7.2.2"
               , "xhtml ==3000.2.2.1"
               ]
