{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import Config.Compiler
import Config.Flags
import Config.PackageSet ( packageSet, targetPackages )
import Config.Tools ( ppPackageSet )
import MyCabal
import Types

-- import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map.Strict ( toList )
import qualified Data.Set as Set
import Development.Shake.Command ( CmdOption(AddEnv), Stdout(..), cmd )
import Data.List ( intercalate )
import qualified Nix.Paths as Nix
import Text.PrettyPrint ( render )

-- | Generate a @cabal install --dry-run@ command that resolves the given target
-- packages under the constraints of our package set.
--
-- TODO: Honor deprecated packages in the Hackage tarball.

updatePackageSet :: FilePath -> Set.Set PackageName -> [String]
updatePackageSet ghc targets = ["cabal", "--store=/var/empty", "install", "--ignore-project", "--lib", "--dry-run", "--with-compiler=" <> ghc, "--minimize-conflict-set", "--upgrade-dependencies"]
                           ++ installed ++ lowerBounds ++ constraints ++ flags ++ allowNewer ++ pkgs
  where
    pkgs = display <$> Set.toList targets
    constraints = [ constraint (display (PackageVersionConstraint pn vr))
                  | (pn, vr) <- toList targetPackages
                  ]
    lowerBounds = [ constraint (unPackageName pn <> " >= " <> display v)
                  | (pn, v) <- toList packageSet
                  ]
    installed = [ constraint (unPackageName pn <> " == " <> display v)
                | (pn, v) <- toList ghcCorePackages
                ]
    flags = [ constraint (unwords [pn, flags']) | (pn, flags') <- flagList ]
    allowNewer = [ "--allow-newer=" <> unPackageName x <> ":" <> unPackageName y
                 | (x, ys) <- jailbreaks, y <- ys
                 ]
    constraint c = "--constraint=any." <> c

-- | Resolve 'targetPackages' and print the resulting package set.
--
-- A plan computed by @cabal install@ contains only the libraries of all
-- dependencies, but we build every package completely, including executables
-- and test suites. So we re-run the solver with all packages of the previous
-- plan as targets until the plan no longer changes.

main :: IO ()
main = do
  ghc <- compiler
  pkgConfig <- pkgConfigPath
  let solve targets = do Stdout out <- cmd (AddEnv "PKG_CONFIG_PATH" pkgConfig) (updatePackageSet ghc targets)
                         pure out
  plan <- resolve solve (Map.keysSet targetPackages) Map.empty
  putStrLn (render (ppPackageSet (plan `Map.withoutKeys` Map.keysSet ghcCorePackages)))

-- | Iterate the solver until the plan is stable.

resolve :: (Set.Set PackageName -> IO String) -> Set.Set PackageName -> PackageSet -> IO PackageSet
resolve solve targets previous = do
  plan <- parseInstallPlan <$> solve targets
  if plan == previous
     then pure plan
     else resolve solve (Map.keysSet targetPackages `Set.union` Map.keysSet plan) plan

-- | Extract the package set from the install plan printed by @cabal install
-- --dry-run@. The relevant lines have the form
--
-- >  - aws-0.25.3 (lib) (requires download & build)
--
-- A package that contributes several components (e.g. a library and an
-- executable) appears more than once, but all entries share the same version.
-- Any line that does not match this format is ignored.

parseInstallPlan :: String -> PackageSet
parseInstallPlan out = Map.fromList
  [ (pkgName pid, pkgVersion pid)
  | l <- lines out
  , ("-" : pidStr : _) <- [words l]
  , Just pid <- [simpleParse pidStr]
  ]

-- | Instantiate the compiler from nixpkgs and return the path of its @ghc@
-- binary.

compiler :: IO FilePath
compiler = do
  ghc : _ <- nixBuild "(import <nixpkgs> {}).haskell.compiler.ghc914"
  pure (ghc <> "/bin/ghc")

-- | System libraries (as nixpkgs attribute names) that packages in our set
-- look up via pkg-config, depending on the flags we choose.

pkgConfigDependencies :: [String]
pkgConfigDependencies = [ "libyaml", "sqlite", "icu", "pango", "cairo", "glib" ]

-- | Instantiate all 'pkgConfigDependencies' from nixpkgs and return a search
-- path for pkg-config that covers their @.pc@ files.

pkgConfigPath :: IO String
pkgConfigPath = do
  paths <- nixBuild ("with import <nixpkgs> {}; map lib.getDev [ " <> unwords pkgConfigDependencies <> " ]")
  pure $ intercalate ":" [ p <> d | p <- paths, d <- ["/lib/pkgconfig", "/share/pkgconfig"] ]

-- | Build a nix expression and return the store paths of its outputs.

nixBuild :: String -> IO [FilePath]
nixBuild expr = do
  Stdout out <- cmd Nix.nixBuild (["--no-out-link", "-E", expr] :: [String])
  pure (lines out)

-- | Packages whose version bounds on the listed dependencies we ignore when
-- resolving the package set. Each of these needs a corresponding patch for
-- the build, e.g. one that relaxes the bounds with @cabal-tweak-dep-ver@.

jailbreaks :: [(PackageName, [PackageName])]
jailbreaks = [ ("botan-bindings", ["base"])
             , ("botan-low", ["base"])
             , ("cabal-install", ["open-browser"])
             , ("cairo", ["Cabal"])
             , ("cborg", ["base", "containers"])
             , ("feed", ["base", "base-compat", "time"])
             , ("ghc-hs-meta", ["base", "ghc-boot", "ghc", "template-haskell"])
             , ("git-lfs", ["containers"])
             , ("glib", ["containers", "Cabal"])
             , ("pango", ["containers", "glib", "Cabal"])
             , ("persistent", ["template-haskell"])
             , ("serialise", ["base", "containers", "time"])
             , ("ShellCheck", ["QuickCheck", "Diff"])
             , ("wizards", ["containers"])
             , ("xmobar", ["base"])
             ]
