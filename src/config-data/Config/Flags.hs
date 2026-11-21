module Config.Flags ( buildFlags ) where

import MyCabal

import qualified Data.List as List
import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map
import Data.Maybe

-- | The flags we configure for packages of our package set. Packages that
-- aren't listed are built with their default flags.

buildFlags :: Map PackageName FlagAssignment
buildFlags = readFlagAssignments
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

    -- Enable all extensions except with_datezone. all_extensions would enable
    -- datezone regardless of the with_datezone flag, so we list the flags it
    -- implies (as of xmobar 0.51.1) explicitly instead.
  , ("xmobar",                         "+with_alsa +with_dbus +with_inotify +with_mpd +with_mpris +with_nl80211 +with_weather +with_xpm -with_datezone")

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
  , ("yaml",                            "+no-examples +no-exe")

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

-- | Parse a list of package names and flag assignments. The assignments use
-- Cabal's syntax, e.g. @"+exe -server"@. Aborts with an 'error' if an
-- assignment can't be parsed, or if a package is listed more than once,
-- because converting the list into a map would silently keep only the last of
-- those entries.
--
-- Every flag must start with @+@ or @-@:
--
-- >>> display <$> readFlagAssignments [("foo", "+bar -baz")]
-- fromList [(PackageName "foo","+bar -baz")]
--
-- >>> readFlagAssignments [("foo", "bar")]
-- fromList *** Exception: foo: invalid flag assignment: ...
-- unexpected 'b'
-- expecting "+", "-", white space or end of input
-- ...
--
-- >>> readFlagAssignments [("foo", "+bar baz")]
-- fromList *** Exception: foo: invalid flag assignment: ...
-- unexpected 'b'
-- expecting space, "+", "-", white space or end of input
-- ...

readFlagAssignments :: [(String,String)] -> Map PackageName FlagAssignment
readFlagAssignments xs
  | not (null duplicates) = error ("duplicate entries in flag list: " <> List.intercalate ", " duplicates)
  | otherwise             = Map.fromList [ (fromJust (simpleParse name), parseFlags name fs) | (name, fs) <- xs ]
  where
    duplicates = [ name | name : _ : _ <- List.group (List.sort (fst <$> xs)) ]
    parseFlags name fs = either (\err -> error (name <> ": invalid flag assignment: " <> err)) id (eitherParsec fs)
