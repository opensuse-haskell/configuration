{-# LANGUAGE OverloadedStrings #-}

module Config.LTS11 ( lts11 ) where

import Config.LTS11.Stackage
import Types
import Orphans ()

import Data.Maybe
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Simple.Utils ( lowercase )
import Distribution.Text
import Distribution.Version

lts11 :: PackageSetConfig
lts11 = PackageSetConfig
        { compiler = "ghc-8.2.2"
        , stackagePackages = filter goodStackagePackage stackage
        , extraPackages = extraPackageNames
        , bannedPackages = bannedPackageNames
        , flagAssignments = readFlagAssignents flagList
        , forcedExectables = forcedExectableNames
        }

goodStackagePackage :: Dependency ->  Bool
goodStackagePackage (Dependency _ v) = v /= noVersion

extraPackageNames :: [Dependency]
extraPackageNames =
  [ -- Used by psimons@suse.com.
    "applicative-quoters"
  , "BNFC"
  , "cabal2spec <= 2.0.1"

    -- Used by ptrommler@icloud.com.
  , "lhs2tex"

    -- Needed for lambdabot libraries and MCP.
  , "lambdabot-reference-plugins"
  , "lambdabot-social-plugins"

    -- Used by osukup@suse.com.
  , "cab"
  , "xmobar", "iwlib"

    -- Needed by games repository somewhere.
  , "SDL-image"
  , "SDL-mixer"

    -- These packages are dependencies of cabal-install.
  , "gnuidn"
  , "gnutls"
  , "gsasl"
  , "network-protocol-xmpp"
  , "regex-tdfa-rc"

    -- git-annex and its dependencies are currently broken, most due to
    -- https://github.com/bitemyapp/esqueleto/issues/80.
  {-
  , "git-annex", "IfElse", "bloomfilter", "esqueleto", "fdo-notify", "magic", "sandi", "torrent"
  -}

    -- These packages are in Factory for historical reasons.
  , "AC-Vector"
  , "acid-state"
  , "aeson-lens"
  , "bool-extras"
  , "bytestring-handle"
  , "chell"
  , "clustering"
  , "concatenative"
  , "continued-fractions"
  , "converge"
  , "conversion"
  , "conversion-bytestring"
  , "conversion-case-insensitive"
  , "conversion-text"
  , "cryptohash-conduit"
  , "doctest-prop"
  , "effin"
  , "enummapset-th"
  , "foldl-statistics"
  , "fortran-src"
  , "freer"
  , "fuzzy"
  , "gamma"
  , "ghc-heap-view"
  , "gio"
  , "GPipe-GLFW"
  , "GraphSCC"
  , "gtk"
  , "gtk3"
  , "hcwiid"
  , "hflags"
  , "hgal"
  , "hjpath"
  , "hjson"
  , "httpd-shed"
  , "imm"
  , "intero"
  , "json-ast"
  , "language-lua2"
  , "lazy-csv"
  , "libnotify"
  , "list-fusion-probe"
  , "mersenne-random"
  , "microbench"
  , "monad-primitive"
  , "monadLib"
  , "multiset-comb"
  , "murmur3"
  , "mwc-random-monad"
  , "nationstates"
  , "network-uri-flag"
  , "numeric-quest"
  , "Octree"
  , "osdkeys"
  , "pbkdf"
  , "pcap"
  , "permutation"
  , "pipes-cliff"
  , "pipes-http"
  , "plot-gtk"
  , "plot-gtk3"
  , "prednote"
  , "presburger"
  , "pure-cdb"
  , "pwstore-purehaskell"
  , "ReadArgs"
  , "readline"
  , "rosezipper"
  , "SDL"
  , "secp256k1"
  , "serversession-backend-acid-state"
  , "show-type"
  , "simple-smt"
  , "smtLib"
  , "sound-collage"
  , "split-record"
  , "spool"
  , "spoon"
  , "state-plus"
  , "storablevector-carray"
  , "streaming-binary"
  , "streaming-wai"
  , "success"
  , "synthesizer-core"
  , "synthesizer-dimensional"
  , "synthesizer-midi"
  , "syz"
  , "taggy"
  , "taggy-lens"
  , "test-invariant"
  , "tree-view"
  , "Unixutils"
  , "uu-interleaved"
  , "uu-parsinglib"
  , "uuid-orphans"
  , "wai-middleware-verbs < 0.4"
  , "wai-request-spec"
  , "yahoo-finance-api"
  , "yesod-default"
  ]

bannedPackageNames :: [PackageName]
bannedPackageNames =
  [ -- GHC 8.2.2 core packages
    "array"
  , "base"
  , "binary"
  , "bytestring"
  , "Cabal"
  , "containers"
  , "deepseq"
  , "directory"
  , "filepath"
  , "ghc-boot"
  , "ghc-boot-th"
  , "ghci"
  , "ghc-compact"
  , "ghc-prim"
  , "haskeline"
  , "hoopl"
  , "hpc"
  , "integer-gmp"
  , "pretty"
  , "process"
  , "rts"
  , "template-haskell"
  , "terminfo"
  , "time"
  , "transformers"
  , "unix"
  , "xhtml"

    -- doesn't work on 32 bit: https://github.com/YoEight/eventstore/issues/51
  , "eventstore", "eventsource-geteventstore-store"

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

    -- example / tutorial code
  , "luminance-samples"

    -- needs an old version of LLVM
  , "reedsolomon"

    -- does not compile: https://github.com/psibi/tldr-hs/issues/4
  , "tldr"

    -- does not compile
  , "llvm-hs", "accelerate-llvm", "accelerate-llvm-native", "accelerate-llvm-ptx"
  , "accelerate-blas", "accelerate-bignum", "accelerate-examples", "accelerate-fft"

    -- depends on seemingly missing blas and lapack system libraries
  , "lapack-ffi", "blas-carray", "blas-ffi", "lapack-carray"

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

    -- build fails: https://github.com/opensuse-haskell/configuration/pull/18#issuecomment-353990383
  , "lame"

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
  , "hw-hedgehog"
  , "hw-hspec-hedgehog"

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
  , "chart-unit", "perf", "online"

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

    -- Non-free license.
  , "cuda", "nvvm", "cublas", "cufft", "cusolver", "cusparse"

    -- Fails during configure stage.
  , "direct-rocksdb", "tasty-auto"

    -- Can't find cblas.h header.
  , "hmatrix-morpheus"

    -- Can't find xlocale.h on Tumbleweed.
  , "mercury-api"

    -- Build fails, probably because our c++ compiler is too new.
  , "yoga"

    -- Hard-codes linker flags that break the build: https://github.com/serokell/importify/issues/88
  , "importify"

    -- Stackage contains only *some* yi packages, but not all, so we can never
    -- rely on the main package to actually compile in our environment.
  , "yi-core"
  , "yi-frontend-vty"
  , "yi-fuzzy-open"
  , "yi-ireader"
  , "yi-keymap-cua"
  , "yi-keymap-emacs"
  , "yi-keymap-vim"
  , "yi-language"
  , "yi-misc-modes"
  , "yi-mode-haskell"
  , "yi-mode-javascript"
  , "yi-rope"
  , "yi-snippet"
  , "haskell-lsp", "haskell-lsp-client"

    -- build is broken: https://github.com/sbidin/sdl2-mixer/issues/4
  , "sdl2-mixer"

    -- Triggers some GHC bug that results in the build failing due to "shadowed dependencies".
  , "apply-refact"
  ]

forcedExectableNames :: [PackageName]
forcedExectableNames =
  [ "Agda"
  , "alex"
  , "apply-refact"
  , "asciidiagram"
  , "BNFC"
  , "bustle"
  , "c2hs"
  , "cab"
  , "cabal-install"
  , "cabal-rpm"
  , "cabal2nix"
  , "cabal2spec"
  , "clash-ghc"
  , "codex"
  , "cpphs"
  , "cryptol"
  , "darcs"
  , "derive"
  , "diagrams-haddock"
  , "dixi"
  , "doctest"
  , "doctest-discover"
  , "find-clumpiness"
  , "ghc-imported-from"
  , "ghc-mod"
  , "ghcid"
  , "git-annex"
  , "gtk2hs-buildtools"
  , "hackage-mirror"
  , "hackmanager"
  , "handwriting"
  , "hapistrano"
  , "happy"
  , "HaRe"
  , "haskintex"
  , "HaXml"
  , "hdevtools"
  , "hdocs"
  , "highlighting-kate"
  , "hindent"
  , "hledger"
  , "hledger-web"
  , "hlint"
  , "holy-project"
  , "hoogle"
  , "hpack"
  , "hpc-coveralls"
  , "hscolour"
  , "hsdev"
  , "hspec-discover"
  , "hspec-setup"
  , "ide-backend"
  , "idris"
  , "keter"
  , "leksah-server"
  , "lhs2tex"
  , "markdown-unlit"
  , "microformats2-parser"
  , "misfortune"
  , "modify-fasta"
  , "msi-kb-backlit"
  , "omnifmt"
  , "osdkeys"
  , "pandoc"
  , "pointfree"
  , "pointful"
  , "postgresql-schema"
  , "purescript"
  , "quickbench"
  , "shake"
  , "ShellCheck"
  , "stack"
  , "stack-run-auto"
  , "stackage-curator"
  , "stylish-haskell"
  , "texmath"
  , "titlecase"
  , "werewolf"
  , "wordpass"
  , "xmobar"
  , "xmonad"
  , "yi"
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

    -- Compile against the system library, not the one bundled in the Haskell
    -- source tarball.
  , ("cmark",                          "pkgconfig")

    -- Make sure our package compiles with GHC 8.2.2.
  , ("cassava",                        "-bytestring--lt-0_10_4")

    -- Avoid dependency on servant-client-core, which is not in LTS-11.
  , ("alerta",                         "-servant-client-core")

    -- Avoid dependency on aws, which doesn't compile in LTS-11.
  , ("git-annex",                      "-s3")
  ]

readFlagAssignents :: [(String,String)] -> [(PackageName,FlagAssignment)]
readFlagAssignents xs = [ (fromJust (simpleParse name), readFlagList (words assignments)) | (name,assignments) <- xs ]

readFlagList :: [String] -> FlagAssignment
readFlagList = mkFlagAssignment .  map (tagWithValue . noMinusF)
  where
    tagWithValue ('-':fname) = (mkFlagName (lowercase fname), False)
    tagWithValue fname       = (mkFlagName (lowercase fname), True)

    noMinusF :: String -> String
    noMinusF ('-':'f':_) = error "don't use '-f' in flag assignments; just use the flag's name"
    noMinusF x           = x
