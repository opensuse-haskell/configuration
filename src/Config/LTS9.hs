{-# LANGUAGE OverloadedStrings #-}

module Config.LTS9 ( lts9 ) where

import Config.LTS9.Stackage
import Types
import Orphans ()

import Data.Maybe
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Simple.Utils ( lowercase )
import Distribution.Text
import Distribution.Version

lts9 :: PackageSetConfig
lts9 = PackageSetConfig
    { compiler = "ghc-8.0.2"
    , stackagePackages = filter goodStackagePackage stackage
    , extraPackages = extraPackageNames
    , bannedPackages = bannedPackageNames
    , flagAssignments = readFlagAssignents flagList
    , forcedExectables = forcedExectableNames
    }

goodStackagePackage :: Dependency ->  Bool
goodStackagePackage (Dependency "git-annex" _) = False
goodStackagePackage (Dependency "happy" _) = False
goodStackagePackage (Dependency "haskell-names" _) = False
goodStackagePackage (Dependency "hledger" _) = False
goodStackagePackage (Dependency "hledger-api" _) = False
goodStackagePackage (Dependency "hledger-iadd" _) = False
goodStackagePackage (Dependency "hledger-interest" _) = False
goodStackagePackage (Dependency "hledger-lib" _) = False
goodStackagePackage (Dependency "hledger-ui" _) = False
goodStackagePackage (Dependency "hledger-web" _) = False
goodStackagePackage (Dependency "swagger2" _) = False
goodStackagePackage (Dependency "traverse-with-class" _) = False
goodStackagePackage (Dependency _ v) = v /= noVersion

extraPackageNames :: [Dependency]
extraPackageNames =
  [ -- Used by psimons@suse.com.
    "applicative-quoters"
  , "arrows"
  , "BNFC"
  , "json-autotype"
  , "lazysmallcheck"
  , "Stream"
  , "structured-haskell-mode"
  , "hledger-lib", "hledger", "hledger-ui", "hledger-interest"
  , "hledger-api", "hledger-iadd < 1.2.5 || > 1.2.5", "hledger-web"

    -- Needed for lambdabot libraries and MCP.
  , "dependent-sum-template"
  , "lambdabot-core"
  , "lambdabot-irc-plugins"
  , "lambdabot-reference-plugins"
  , "lambdabot-social-plugins"
  , "prim-uniq"

    -- Newer versions require traverse-with-class >=1.0.0.0 && <1.1
  , "haskell-names < 0.9"

    -- Newer versions break the build of hledger-api 1.3.1.
  , "swagger2 >= 2.1 && < 2.1.5"

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
  , "traverse-with-class < 1"
  , "git-annex"

    -- lts-9.x has the newer 1.19.6+, which is supposed to be API compatible
    -- but in fact breaks Agda, cryptol, fortran-src, and language-python.
  , "happy == 1.19.5"

    -- These packages are in Factory for historical reasons.
  , "acid-state"
  , "bzlib"
  , "camfort"
  , "cassava-megaparsec < 1"
  , "ChasingBottoms"
  , "clckwrks"
  , "clckwrks-cli"
  , "clckwrks-plugin-media"
  , "clckwrks-plugin-page"
  , "clckwrks-theme-bootstrap"
  , "concatenative"
  , "concurrent-extra"
  , "continued-fractions"
  , "converge"
  , "conversion"
  , "conversion-bytestring"
  , "conversion-case-insensitive"
  , "conversion-text"
  , "crackNum"
  , "cryptol"
  , "crypto-numbers"
  , "crypto-pubkey"
  , "datasets"
  , "distributed-process"
  , "doctest-prop"
  , "fay"
  , "fay-base"
  , "fay-builder"
  , "fay-uri"
  , "FindBin"
  , "FloatingHex"
  , "fortran-src"
  , "gamma"
  , "GenericPretty"
  , "genvalidity"
  , "genvalidity-hspec"
  , "genvalidity-property"
  , "ghc-mod"
  , "gipeda"
  , "google-translate"
  , "GraphSCC"
  , "happstack-authenticate"
  , "happstack-clientsession"
  , "HaRe"
  , "haskell-names"   -- https://github.com/haskell-suite/haskell-names/issues/97
  , "hflags"
  , "hgal"
  , "hip"
  , "hjpath"
  , "hjson"
  , "hmatrix-repa"
  , "hmt ==0.15"
  , "hOpenPGP"
  , "ixset"
  , "json-ast"
  , "katip-elasticsearch"
  , "lazy-csv"
  , "load-env"
  , "mersenne-random"
  , "microbench"
  , "misfortune"
  , "monadLib"
  , "monad-primitive"
  , "multiset-comb"
  , "murmur3"
  , "mwc-random-monad"
  , "nationstates"
  , "nettle"
  , "network-uri-flag"
  , "pbkdf"
  , "pcap"
  , "permutation"
  , "pipes-aeson"
  , "pipes-cliff"
  , "pipes-csv"
  , "pipes-fastx"
  , "pipes-http"
  , "pipes-network"
  , "presburger"
  , "pure-cdb"
  , "pwstore-purehaskell"
  , "quickcheck-properties"
  , "repa"
  , "repa-algorithms"
  , "repa-io"
  , "riak"
  , "riak-protobuf"
  , "RNAlien"
  , "rosezipper"
  , "sbv"
  , "secp256k1"
  , "serversession-backend-acid-state"
  , "show-type"
  , "simple-smt"
  , "smtLib"
  , "spoon"
  , "state-plus"
  , "stb-image-redux"
  , "stripe-core"
  , "stripe-haskell"
  , "stripe-http-streams"
  , "stripe-tests"
  , "success"
  , "syb-with-class"
  , "syz"
  , "taggy"
  , "taggy-lens"
  , "test-invariant"
  , "test-simple"
  , "th-printf"
  , "tibetan-utils == 0.1.1.2"
  , "tree-view"
  , "type-eq"
  , "unagi-chan"
  , "Unixutils"
  , "userid"
  , "uuid-orphans"
  , "validity"
  , "vector-fftw"
  , "wai-request-spec"
  , "wavefront"
  , "wordpass"
  , "yi < 0.14"
  , "yi-frontend-pango < 0.14"
  ]

bannedPackageNames :: [PackageName]
bannedPackageNames =
  [ -- GHC 8.0.x core packages
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

    -- Don't use the bundled lua library.
  , ("hslua",                          "system-lua")

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

    -- Since version 6.20170925, the test suite can no longer be run outside of
    -- a checked-out copy of the git repository.
  , ("git-annex",                      "-testsuite")

    -- Compile against the system library, not the one bundled in the Haskell
    -- source tarball.
  , ("cmark",                          "pkgconfig")
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