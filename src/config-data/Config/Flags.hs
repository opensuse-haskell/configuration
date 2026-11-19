module Config.Flags ( flagList, readFlagAssignents ) where

import MyCabal

import Data.Maybe

flagList :: [(String,String)]
flagList =
  [ ("cabal-plan",                     "+exe")

    -- Don't build hardware-specific optimizations into the binary based on what the
    -- build machine supports or doesn't support.
  , ("cryptonite",                     "-support_aesni -support_rdrand -support_sse")

    -- Don't use the bundled sqlite3 library.
  , ("direct-sqlite",                  "+systemlib")

    -- dont optimize happy with happy ( dep on same package ..)
  , ("happy",                          "-bootstrap")

    -- Build the standalone executable and prefer pcre-light, which uses the system
    -- library rather than a bundled copy.
  , ("highlighting-kate",              "+executable +pcre-light")

    -- Don't use the bundled sass library.
  , ("hlibsass",                       "+externalLibsass")

    -- Use the bundled lua library. People expect this package to provide LUA
    -- 5.3, but we don't have that yet in openSUSE.
  , ("hslua",                          "-system-lua")

    -- Allow compilation without having Nix installed.
  , ("nix-paths",                      "+allow-relative-paths")

    -- Build the standalone executable.
  , ("texmath",                        "+executable -server")

    -- Enable almost all extensions.
  , ("xmobar",                         "+all_extensions")

    -- Enable additional features
  , ("idris",                          "+ffi +gmp")

    -- Disable dependencies we don't have.
  , ("invertible",                     "-hlist -piso")

    -- Use the system sqlite library rather than the bundled one.
  , ("persistent-sqlite",              "+systemlib")

    -- Make sure we're building with all features enabled.
    -- TODO: The OsPath feature needs file-io >= 0.2.0.
  , ("git-annex",                      "+assistant -benchmark +blake3 +botan +dbus -debuglocks +magicmime -memory -nollmdependencies -ospath +production -tastytap +torrentparser +xxh3")

    -- Compile against the system library, not the one bundled in the Haskell
    -- source tarball.
  , ("cmark",                          "+pkgconfig")

    -- Fix build with modern compilers.
  , ("cassava",                        "-bytestring--lt-0_10_4")

    -- Prefer the system's library over the bundled one.
  , ("libyaml",                        "+system-libyaml")

    -- Don't install example files.
  , ("yaml",                            "-no-examples -no-exe")

    -- Configure a production-like build environment.
  , ("stack",                          "+hide-dependency-versions +disable-git-info +supported-build")

    -- The command-line utility pulls in other dependencies.
  , ("aeson-pretty",                   "+lib-only")

    -- Build the standalone executables for these packages.
  , ("skylighting",                    "+executable")
  , ("citeproc",                       "+executable +icu")

    -- Compile against system zstd library
  , ("zstd",                           "-standalone")

    -- Static linking breaks the build.
  , ("hadolint",                       "-static")

    -- Enable LUA support.
  , ("pandoc-cli",                     "+lua +server")

    -- Disable use of -march=native during compilation.
  , ("hashable",                       "-arch-native")

    -- Don't build and install the 'example' binary.
  , ("open-browser",                   "-example")

    -- This flag switches the dependency on underlying configuration meta
    -- library. We should probably use the pkgconfig version, but our initial
    -- submission to Factory did not, so now we're using the pg_config(1) based
    -- variant for historical reasons.
  , ("postgresql-libpq",               "-use-pkg-config")
  ]

readFlagAssignents :: [(String,String)] -> [(PackageName,FlagAssignment)]
readFlagAssignents xs = [ (fromJust (simpleParse name), readFlagList (words assignments)) | (name,assignments) <- xs ]

readFlagList :: [String] -> FlagAssignment
readFlagList = mkFlagAssignment . map (tagWithValue . specifyPlus . noMinusF)
  where
    tagWithValue ('-':fname) = (mkFlagName (lowercase fname), False)
    tagWithValue ('+':fname) = (mkFlagName (lowercase fname), True)
    tagWithValue fname       = (mkFlagName (lowercase fname), True)

    noMinusF :: String -> String
    noMinusF ('-':'f':_) = error "don't use '-f' in flag assignments; just use the flag's name"
    noMinusF x           = x

    specifyPlus :: String -> String
    specifyPlus ""           = error "specifyPlus: invalid empty Cabal flag"
    specifyPlus x@(c:_)
      | c == '-' || c == '+' = x
      | otherwise            = error ("specifyPlus: flag " ++ show x ++ " must start with '-' or '+'")
