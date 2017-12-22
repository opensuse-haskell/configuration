{-# LANGUAGE OverloadedStrings #-}

module Config.LTS10 ( lts10 ) where

import Config.LTS10.Stackage
import Types
import Orphans ()

import Data.Maybe
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Simple.Utils ( lowercase )
import Distribution.Text
import Distribution.Version

lts10 :: PackageSetConfig
lts10 = PackageSetConfig
        { compiler = "ghc-8.2.2"
        , stackagePackages = filter goodStackagePackage stackage
        , extraPackages = extraPackageNames
        , bannedPackages = bannedPackageNames
        , flagAssignments = readFlagAssignents flagList
        , forcedExectables = forcedExectableNames
        }

goodStackagePackage :: Dependency ->  Bool
goodStackagePackage (Dependency "git-annex" _) = False
goodStackagePackage (Dependency "hledger" _) = False
goodStackagePackage (Dependency "hledger-api" _) = False
goodStackagePackage (Dependency "hledger-iadd" _) = False
goodStackagePackage (Dependency "hledger-interest" _) = False
goodStackagePackage (Dependency "hledger-lib" _) = False
goodStackagePackage (Dependency "hledger-ui" _) = False
goodStackagePackage (Dependency "hledger-web" _) = False
goodStackagePackage (Dependency _ v) = v /= noVersion

extraPackageNames :: [Dependency]
extraPackageNames =
  [ -- Used by psimons@suse.com.
    "applicative-quoters"
  , "BNFC"
  , "hledger-lib", "hledger", "hledger-ui", "hledger-interest"
  , "hledger-api", "hledger-iadd < 1.2.5 || > 1.2.5", "hledger-web"

    -- Needed for lambdabot libraries and MCP.
  , "lambdabot-core"
  , "lambdabot-irc-plugins"
  , "lambdabot-reference-plugins"
  , "lambdabot-social-plugins"
  , "prim-uniq"

    -- Used by osukup@suse.com.
  , "cab"
  , "pointfree"
  , "xmobar"

    -- Needed by games repository somewhere.
  , "SDL-image"
  , "SDL-mixer"

    -- These packages are dependencies of cabal-install.
  , "data-default-instances-base"
  , "gnuidn"
  , "gnutls"
  , "gsasl"
  , "network-protocol-xmpp"
  , "regex-tdfa-rc"

    -- Re-add these packages with modified constrains.
  , "git-annex"

    -- These packages are in Factory for historical reasons.
  , "acid-state"
  , "ChasingBottoms"
  , "clckwrks"
  , "clckwrks-cli"
  , "clckwrks-plugin-media"
  , "clckwrks-plugin-page"
  , "clckwrks-theme-bootstrap"
  , "concatenative"
  , "continued-fractions"
  , "converge"
  , "conversion"
  , "conversion-bytestring"
  , "conversion-case-insensitive"
  , "conversion-text"
  , "doctest-prop"
  , "fay"
  , "fay-base"
  , "fay-builder"
  , "fay-uri"
  , "fortran-src"
  , "gamma"
  , "ghc-mod"
  , "gipeda"
  , "GraphSCC"
  , "HaRe"
  , "hflags"
  , "hgal"
  , "hjpath"
  , "hjson"
  , "hmt ==0.15"
  , "json-ast"
  , "lazy-csv"
  , "mersenne-random"
  , "microbench"
  , "monadLib"
  , "monad-primitive"
  , "multiset-comb"
  , "murmur3"
  , "mwc-random-monad"
  , "nationstates"
  , "network-uri-flag"
  , "pbkdf"
  , "pcap"
  , "permutation"
  , "pipes-cliff"
  , "pipes-http"
  , "presburger"
  , "pure-cdb"
  , "pwstore-purehaskell"
  , "rosezipper"
  , "secp256k1"
  , "serversession-backend-acid-state"
  , "show-type"
  , "simple-smt"
  , "smtLib"
  , "spoon"
  , "state-plus"
  , "stb-image-redux"
  , "success"
  , "syz"
  , "taggy"
  , "taggy-lens"
  , "test-invariant"
  , "tree-view"
  , "type-eq"
  , "Unixutils"
  , "uuid-orphans"
  , "wai-request-spec"
  , "yi < 0.14"
  , "yi-frontend-pango < 0.14"

  -- TODO: These packages are still in d:l:h, but have disappeared from LTS-10.
  -- , "arbtt"
  -- , "BlogLiterately"
  -- , "bumper"
  -- , "cabal-dependency-licenses"
  -- , "clash-ghc"
  -- , "cryptol"
  -- , "darcs"
  -- , "dmenu-pkill"
  -- , "dmenu-pmount"
  -- , "dmenu-search"
  -- , "AC-Vector"
  -- , "aeson-lens"
  -- , "amazonka-s3-streaming"
  -- , "anonymous-sums"
  -- , "attoparsec-time"
  -- , "avers-api-docs"
  -- , "base-noprelude"
  -- , "binary-typed"
  -- , "BlogLiterately-diagrams"
  -- , "board-games"
  -- , "bool-extras"
  -- , "bytestring-handle"
  -- , "bytestring-progress"
  -- , "cabal-helper"
  -- , "cacophony"
  -- , "camfort"
  -- , "cgi"
  -- , "chell"
  -- , "clang-pure"
  -- , "clash-lib"
  -- , "clash-prelude"
  -- , "clash-systemverilog"
  -- , "clash-verilog"
  -- , "clash-vhdl"
  -- , "clustering"
  -- , "cryptohash-conduit"
  -- , "diagrams-gtk"
  -- , "diagrams-html5"
  -- , "diagrams-rasterific"
  -- , "discord-gateway"
  -- , "discord-hs"
  -- , "discord-rest"
  -- , "discord-types"
  -- , "dmenu"
  -- , "effin"
  -- , "elm-bridge"
  -- , "encoding-io"
  -- , "engine-io"
  -- , "engine-io-wai"
  -- , "enummapset-th"
  -- , "etc"
  -- , "euphoria"
  -- , "extract-dependencies"
  -- , "fgl-arbitrary"
  -- , "flock"
  -- , "foldl-statistics"
  -- , "freer"
  -- , "freer-effects"
  -- , "fuzzy"
  -- , "ghc-heap-view"
  -- , "gi-gdk"
  -- , "gi-gdkpixbuf"
  -- , "gi-gio"
  -- , "gi-gtk"
  -- , "gio"
  -- , "gi-pango"
  -- , "gi-soup"
  -- , "gitson"
  -- , "glazier-react"
  -- , "glazier-react-widget"
  -- , "GPipe-GLFW"
  -- , "gtk"
  -- , "gtk3"
  -- , "haddock-api"
  -- , "haddock-test"
  -- , "hakyll-favicon"
  -- , "haskell-packages"
  -- , "hatex-guide"
  -- , "hcwiid"
  -- , "highlight"
  -- , "hinterface"
  -- , "hPDB"
  -- , "httpd-shed"
  -- , "imm"
  -- , "isotope"
  -- , "language-dockerfile"
  -- , "language-ecmascript"
  -- , "language-lua2"
  -- , "language-python"
  -- , "language-thrift"
  -- , "libnotify"
  -- , "librato"
  -- , "list-fusion-probe"
  -- , "lucid-svg"
  -- , "machines-process"
  -- , "mediabus"
  -- , "mediabus-rtp"
  -- , "numeric-quest"
  -- , "oanda-rest-api"
  -- , "octane"
  -- , "Octree"
  -- , "optparse-helper"
  -- , "overloaded-records"
  -- , "persistent-redis"
  -- , "pinchot"
  -- , "pipes-cacophony"
  -- , "plot-gtk"
  -- , "plot-gtk3"
  -- , "plot-gtk-ui"
  -- , "plot-light"
  -- , "point-octree"
  -- , "posix-realtime"
  -- , "postgresql-simple-opts"
  -- , "prednote"
  -- , "questioner"
  -- , "ReadArgs"
  -- , "readline"
  -- , "regex"
  -- , "regex-with-pcre"
  -- , "RepLib"
  -- , "rerebase"
  -- , "rethinkdb"
  -- , "rotating-log"
  -- , "rss-conduit"
  -- , "scrape-changes"
  -- , "SDL"
  -- , "sibe"
  -- , "simple-download"
  -- , "smallcaps"
  -- , "smallcheck-series"
  -- , "solga"
  -- , "solga-swagger"
  -- , "spool"
  -- , "static-canvas"
  -- , "stemmer"
  -- , "storablevector-carray"
  -- , "streaming-binary"
  -- , "streaming-utils"
  -- , "streaming-wai"
  -- , "superrecord"
  -- , "synthesizer-core"
  -- , "synthesizer-dimensional"
  -- , "synthesizer-midi"
  -- , "tagshare"
  -- , "tdigest-Chart"
  -- , "testing-feat"
  -- , "tracy"
  -- , "twitter-feed"
  -- , "type-list"
  -- , "unbound"
  -- , "units"
  -- , "units-defs"
  -- , "uu-interleaved"
  -- , "uu-parsinglib"
  -- , "vinyl-utils"
  -- , "wai-middleware-content-type"
  -- , "wai-middleware-verbs"
  -- , "wai-routes"
  -- , "waitra"
  -- , "wild-bind-indicator"
  -- , "wild-bind-task-x11"
  -- , "yahoo-finance-api"
  -- , "yesod-auth-account"
  -- , "yesod-default"
  -- , "yesod-job-queue"
  -- , "yesod-markdown"
  -- , "haddock"
  -- , "hdocs"
  -- , "hPDB-examples"
  -- , "hsdev"
  -- , "hspec-setup"
  -- , "idris"
  -- , "intero"
  -- , "keter"
  -- , "osdkeys"
  -- , "packunused"
  -- , "patat"
  -- , "patterns-haskell"
  -- , "resolve-trivial-conflicts"
  -- , "sound-collage"
  -- , "split-record"
  -- , "stack-run-auto"
  -- , "tttool"
  -- , "xdcc"
  -- , "yackage"
  -- , "yesod-bin"
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
  , "eventstore"

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

    -- depends on libmp3lame-devel, which exists only on packman
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

    -- Non-free license.
  , "cuda"

    -- Fails during configure stage.
  , "direct-rocksdb", "tasty-auto"

    -- Can't find cblas.h header.
  , "hmatrix-morpheus"

    -- Can't find xlocale.h on Tumbleweed.
  , "mercury-api"
  ]

forcedExectableNames :: [PackageName]
forcedExectableNames =
  [ "Agda"
  , "alex"
  , "apply-refact"
  , "asciidiagram"
  , "BlogLiterately"
  , "BNFC"
  , "bustle"
  , "c2hs"
  , "cab"
  , "cabal2nix"
  , "cabal-install"
  , "cabal-rpm"
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
  , "ghcid"
  , "ghc-imported-from"
  , "ghc-mod"
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
  , "stackage-curator"
  , "stack-run-auto"
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
  ]

readFlagAssignents :: [(String,String)] -> [(PackageName,FlagAssignment)]
readFlagAssignents xs = [ (fromJust (simpleParse name), readFlagList (words assignments)) | (name,assignments) <- xs ]

readFlagList :: [String] -> FlagAssignment
readFlagList = map (tagWithValue . noMinusF)
  where
    tagWithValue ('-':fname) = (mkFlagName (lowercase fname), False)
    tagWithValue fname       = (mkFlagName (lowercase fname), True)

    noMinusF :: String -> String
    noMinusF ('-':'f':_) = error "don't use '-f' in flag assignments; just use the flag's name"
    noMinusF x           = x
