{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Config.Ghc88x ( ghc88x ) where

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

ghc88x :: Action PackageSetConfig
ghc88x = do
  let compiler = "ghc-8.8.1"
      flagAssignments = fromList (readFlagAssignents flagList)
      forcedExectables = forcedExectableNames
  packageSet <- fromList <$>
                  forM (toList constraintList) (\(pn,vr) ->
                    (,) pn <$> askOracle (Dependency pn vr))
  pure (PackageSetConfig {..})

targetPackages :: ConstraintSet
targetPackages   = [ "alex"
                   , "happy"
                   , "SDL >=0.6.6.0"        -- Dmitriy Perlow <dap.darkness@gmail.com> needs the
                   , "SDL-image >=0.6.1.2"  -- SDL packages for games/raincat.
                   , "SDL-mixer >=0.6.2.0"
                   ]

resolveConstraints :: String
resolveConstraints = unwords ["cabal", "new-install", "--dry-run", constraints, flags, pkgs]
  where
    pkgs = intercalate " " (display <$> keys targetPackages)
    constraints = "--constraint=" <> intercalate " --constraint=" (show <$> environment)
    environment = display . uncurry Dependency <$> toList (corePackages `union` targetPackages)
    flags = unwords [ "--constraint=" <> show (unwords [unPackageName pn, flags])
                    | pn <- keys targetPackages
                    , Just flags <- [lookup (unPackageName pn) flagList]
                    ]

constraintList :: ConstraintSet
constraintList = [ "alex"
                 , "happy"
                 , "SDL >=0.6.6.0"
                 , "SDL-image >=0.6.1.2"
                 , "SDL-mixer >=0.6.2.0"
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
               , "base ==4.13.0.0"
               , "binary ==0.8.6.0"
               , "bytestring ==0.10.9.0"
               , "Cabal ==2.5.0.0"
               , "containers ==0.6.0.1"
               , "deepseq ==1.4.4.0"
               , "directory ==1.3.3.2"
               , "filepath ==1.4.2.1"
               , "ghc ==8.8.0.20190424"
               , "ghc-boot ==8.8.0.20190424"
               , "ghc-boot-th ==8.8.0.20190424"
               , "ghc-compact ==0.1.0.0"
               , "ghc-heap ==8.8.0.20190424"
               , "ghc-prim ==0.5.3"
               , "ghci ==8.8.0.20190424"
               , "haskeline ==0.7.5.0"
               , "hpc ==0.6.0.3"
               , "integer-gmp ==1.0.2.0"
               , "libiserv ==8.8.0.20190424"
               , "mtl ==2.2.2"
               , "parsec ==3.1.13.0"
               , "pretty ==1.1.3.6"
               , "process ==1.6.5.0"
               , "rts ==1.0"
               , "stm ==2.5.0.0"
               , "template-haskell ==2.15.0.0"
               , "terminfo ==0.4.1.3"
               , "text ==1.2.3.1"
               , "time ==1.9.2"
               , "transformers ==0.5.6.2"
               , "unix ==2.7.2.2"
               , "xhtml ==3000.2.2.1"
               ]
