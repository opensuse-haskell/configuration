{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Config.Compiler ( ghcCompiler, ghcCorePackages ) where

import Types
import MyCabal

ghcCompiler :: CompilerId
ghcCompiler = "ghc-9.14.1"

ghcCorePackages :: PackageSet
ghcCorePackages = [ "array-0.5.8.0"
                  , "base-4.22.0.0"
                  , "binary-0.8.9.3"
                  , "bytestring-0.12.2.0"
                  , "Cabal-3.16.0.0"
                  , "Cabal-syntax-3.16.0.0"
                  , "containers-0.8"
                  , "deepseq-1.5.1.0"
                  , "directory-1.3.10.0"
                  , "exceptions-0.10.11"
                  , "file-io-0.1.5"
                  , "filepath-1.5.4.0"
                  , "ghc-9.14.1"
                  , "ghc-bignum-1.4"
                  , "ghc-boot-9.14.1"
                  , "ghc-boot-th-9.14.1"
                  , "ghc-compact-0.1.0.0"
                  , "ghc-experimental-9.1401.0"
                  , "ghc-heap-9.14.1"
                  , "ghc-internal-9.1401.0"
                  , "ghc-platform-0.1.0.0"
                  , "ghc-prim-0.13.1"
                  , "ghc-toolchain-0.1.0.0"
                  , "ghci-9.14.1"
                  , "haddock-api-2.33.0"
                  , "haddock-library-1.11.0"
                  , "haskeline-0.8.3.0"
                  , "hpc-0.7.0.2"
                  , "integer-gmp-1.1"
                  , "mtl-2.3.1"
                  , "os-string-2.0.8"
                  , "parsec-3.1.18.0"
                  , "pretty-1.1.3.6"
                  , "process-1.6.26.1"
                  , "rts-1.0.3"
                  , "semaphore-compat-1.0.0"
                  , "stm-2.5.3.1"
                  , "system-cxx-std-lib-1.0"
                  , "template-haskell-2.24.0.0"
                  , "template-haskell-lift-0.1.0.0"
                  , "template-haskell-quasiquoter-0.1.0.0"
                  , "terminfo-0.4.1.7"
                  , "text-2.1.3"
                  , "time-1.15"
                  , "transformers-0.6.1.2"
                  , "unix-2.8.8.0"
                  , "xhtml-3000.2.2.1"

                  -- This package is a part of ghc-compiler."
                  , "hsc2hs-0.68.10"
                  ]
