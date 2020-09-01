{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Config.LTS14 ( lts14 ) where

import Config.ForcedExecutables
import Config.LTS14.Stackage
import Oracle.Hackage ( )
import Orphans ()
import Types

import Control.Monad
import Data.Map.Strict as Map ( fromList, toList, union, withoutKeys, keysSet )
import Data.Maybe
import Data.Set ( Set )
import Development.Shake
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Simple.Utils ( lowercase )
import Distribution.Text
import Distribution.Types.PackageVersionConstraint

lts14 :: Action PackageSetConfig
lts14 = do
  let compiler = "ghc-8.6.5"
      flagAssignments = fromList (readFlagAssignents flagList)
      forcedExectables = forcedExectableNames
      myConstraintSet = extraPackages `union` ((stackage `withoutKeys` bannedPackageNames) `withoutKeys` keysSet corePackages)
      corePackages    = ghcCorePackages
  packageSet <- fromList <$>
                  forM (toList myConstraintSet) (\(pn,vr) ->
                    (,) pn <$> askOracle (PackageVersionConstraint pn vr))
  pure (PackageSetConfig {..})

extraPackages :: ConstraintSet
extraPackages =
  [ -- Used by osukup@suse.com - xmonad + xmobar + taffybar + needed dependencies
    "ConfigFile"
  , "dbus-hslogger"
  , "gi-dbusmenu"
  , "gi-dbusmenugtk3"
  , "gi-gdkx11"
  , "gi-xlib"
  , "gtk-sni-tray"
  , "gtk-strut"
  , "parsec-numbers"
  , "rate-limit"
  , "spool"
  , "status-notifier-item"
  , "taffybar"
  , "time-units"
  , "xml-helpers"
  , "xmobar"
  , "xmonad"
  , "xmonad-contrib"

    -- LTS-14.x has 2.4.0.1, which we cannot build because we're stuck with the
    -- compiler's native Cabal library.
  , "cabal-install == 2.4.0.0"
  ]

bannedPackageNames :: Set PackageName
bannedPackageNames =
  [ -- doesn't work on 32 bit: https://github.com/YoEight/eventstore/issues/51
    "eventstore"

    -- needs obsolete dependencies we don't want to provide
  , "cabal2nix"

    -- we have no working freenect
  , "freenect"

    -- no license information: https://github.com/wereHamster/github-types/issues/2
  , "github-types"

    -- depends on broken github-types
  , "github-webhook-handler"

    -- depends on broken github-types
  , "github-webhook-handler-snap"

    -- https://github.com/fpco/stackage/issues/2668
  , "h2c"
  , "bno055-haskell"

    -- doesn't work on Linux
  , "hfsevents"

    -- we don't have libocilib.so
  , "hocilib"

    -- depends on broken wl-pprint-terminfo
  , "hopenpgp-tools"

    -- deprecated
  , "ide-backend-rts"

    -- build is hard to fix
  , "lhs2tex"

    -- example / tutorial code
  , "luminance-samples"

    -- needs an old version of LLVM
  , "reedsolomon"

    -- does not compile: https://github.com/psibi/tldr-hs/issues/4
  , "tldr"

    -- does not compile
  , "llvm-hs"

    -- doesn't work on 32 bit: https://github.com/eurekagenomics/SeqAlign/issues/2
  , "seqalign"

    -- no dummy packages here
  , "stackage"

    -- deprecated
  , "stackage-build-plan"
  , "stackage-cabal"
  , "stackage-cli"
  , "stackage-sandbox"
  , "stackage-setup"
  , "stackage-types"
  , "stackage-query"

    -- doesn't work on Linux
  , "Win32"

    -- doesn't work on Linux
  , "Win32-extras"

    -- doesn't work on Linux
  , "Win32-notify"

    -- doesn't work on Linux
  , "clr-host", "clr-inline", "clr-marshal"

    -- https://github.com/opensuse-haskell/configuration/issues/7
  , "wl-pprint-terminfo"

    -- doesn't have a proper license
  , "timeit"

    -- we don't have libtensor
  , "tensorflow"
  , "tensorflow-core-ops"
  , "tensorflow-opgen"
  , "tensorflow-ops"
  , "tensorflow-proto"
  , "tensorflow-test"

    -- This package needs libjvm, but that library is hidden in a strange location
    -- (/usr/lib64/jvm/java-1.8.0-openjdk-1.8.0/jre/lib/amd64/server/libjvm.so) and
    -- I don't how to refer to that path in a portable way, i.e. without hardcoding
    -- specific version and implementation of JDK.
  , "jni"
  , "inline-java"
  , "jvm"
  , "jvm-streaming"
  , "sparkle"
  , "jvm-batching"

    -- Its license (Free Public License 1.0.0 a.k.a. FPL-1.0) is not listed by SPDX.
    -- We probably *could* use 0BSD for these packages.
  , "cql"
  , "cql-io"
  , "wai-predicates"
  , "wai-routing"

    -- It's unclear which license this package is under.
  , "parsec-numeric"

    -- Doesn't work on 32 bit: https://github.com/haskell-works/hw-bits/issues/1.
    -- Upstream is unresponsive. Poor documentation.
  , "hw-balancedparens"
  , "hw-bits"
  , "hw-conduit"
  , "hw-diagnostics"
  , "hw-excess"
  , "hw-int"
  , "hw-json"
  , "hw-mquery"
  , "hw-parser"
  , "hw-prim"
  , "hw-rankselect"
  , "hw-rankselect-base"
  , "hw-string-parse"
  , "hw-succinct"
  , "hw-xml"

    -- Removed packages that failed to build in d:l:h and which are no longer
    -- in lts-8.x anyway.
  , "atndapi"
  , "bitcoin-payment-channel"
  , "bustle"
  , "compdata"
  , "forecast-io"
  , "haskoin-core"
  , "hgettext"
  , "hworker-ses"
  , "hworker-ses"
  , "pipes-bgzf"
  , "pipes-bgzf"
  , "pipes-illumina"
  , "postgresql-schema"
  , "rbpcp-api"

    -- Does not compile on i686 and upstream is unresponsive.
    -- https://github.com/factisresearch/large-hashable/issues/11
  , "large-hashable"

    -- Uses Facebook's "patent termination clause".
    -- https://github.com/facebook/Haxl/issues/67
  , "haxl"
  , "haxl-amazonka"

    -- Early alpha-quality software without documentation.
  , "chart-unit", "perf"

    -- Obsolete package.
  , "groupoids"

    -- Looks very immature.
  , "proto-lens-combinators"

    -- hscurses doesn't work reliably, probably a problem on our side.
  , "hscurses"
  , "mushu"

    -- Ambiguous licensing information: https://github.com/yamadapc/list-prompt/pull/2
  , "list-prompt"

    -- Has no documentation and virtually no code content. Isn't used by anyone either.
  , "dataurl"

    -- Shitty documentation. It's unclear how this code relates to the wavefront library.
  , "wavefront-obj"

    -- Inconsistent license: https://github.com/transient-haskell/transient-universe/issues/34
  , "transient-universe"
  , "ghcjs-hplay"
  , "axiom"

    -- We don't have symengine.
  , "symengine", "symengine-hs"

    -- These packages no longer compile with recent versions of servant.
  , "xlsior", "telegram-api"

    -- These builds are broken in recent versions of Factory and we don't know why.
  , "download", "convert-annotation", "H", "inline-r"

    -- Conflict in "/usr/bin" with other packages.
  , "dice", "protobuf-simple", "postgresql-simple-migration", "proto-lens-protobuf-types"

    -- These packages depend on the obsolete, unmaintained webkitgtk-3.0 library.
  , "gi-webkit", "gi-javascriptcore", "webkitgtk3", "webkitgtk3-javascriptcore"

    -- Current version don't compile with up-to-date dependencies.
  , "distributed-process-simplelocalnet"
  , "fay-dom", "yesod-fay", "fay-text", "fay-jquery"

    -- We don't have liboath.
  , "liboath-hs"

    -- Does not compile with recent versions of glibc.
  , "mercury-api"

    -- Does not compile.
  , "yoga", "cuda", "modular", "termbox", "transformers-lift", "hmatrix-backprop"
  , "esqueleto", "emd", "bins", "refined"

    -- Need pkgconfig(blas), which we don't have.
  , "blas-ffi", "blas-carray"

    -- Need pkgconfig(lapack), which we don't have.
  , "lapack-ffi", "lapack-carray"

    -- https://github.com/fedora-haskell/ghc-rpm-macros/issues/9
  , "s3-signer"

    -- Works only with msodbc, which we don't have.
  , "odbc"

    -- No proper license, no upstream contact details.
  , "peano", "natural-induction", "Fin"

    -- Unrecognizable license
  , "exomizer"
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

    -- Enable some extensions. There are plenty more, but these need more
    -- testing and/or fixing before they work with ghc 8.4.x.
  , ("xmobar",                         "with_utf8 with_xft with_xpm with_thread")

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

    -- Avoid dependency on 'cborg' and/or 'serialise'.
  , ("exinst",                         "-serialise")
  , ("safe-money",                     "-serialise")
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
ghcCorePackages = [ "array-0.5.3.0"
                  , "base-4.12.0.0"
                  , "binary-0.8.6.0"
                  , "bytestring-0.10.8.2"
                  , "Cabal-2.4.0.1"
                  , "containers-0.6.0.1"
                  , "deepseq-1.4.4.0"
                  , "directory-1.3.3.0"
                  , "filepath-1.4.2.1"
                  , "ghc-8.6.5"
                  , "ghc-boot-8.6.5"
                  , "ghc-boot-th-8.6.5"
                  , "ghc-compact-0.1.0.0"
                  , "ghc-heap-8.6.5"
                  , "ghc-prim-0.5.3"
                  , "ghci-8.6.5"
                  , "haskeline-0.7.4.3"
                  , "hpc-0.6.0.3"
                  , "integer-gmp-1.0.2.0"
                  , "libiserv-8.6.3"
                  , "mtl-2.2.2"
                  , "parsec-3.1.13.0"
                  , "pretty-1.1.3.6"
                  , "process-1.6.5.0"
                  , "rts-1.0"
                  , "stm-2.5.0.0"
                  , "template-haskell-2.14.0.0"
                  , "terminfo-0.4.1.2"
                  , "text-1.2.3.1"
                  , "time-1.8.0.2"
                  , "transformers-0.5.6.2"
                  , "unix-2.7.2.2"
                  , "xhtml-3000.2.2.1"
                  ]
