{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import Config ( PackageSetSpec(allowNewer, corePackages, flags, packageSet, pkgConfigDependencies, targetPackages), packageSetSpec, ppPackageSet )
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

updatePackageSet :: FilePath -> PackageNameSet -> [String]
updatePackageSet ghc targets = ["cabal", "--store=/var/empty", "install", "--ignore-project", "--lib", "--dry-run", "--with-compiler=" <> ghc, "--minimize-conflict-set", "--upgrade-dependencies"]
                           ++ installed ++ lowerBounds ++ constraints ++ flagOpts ++ allowNewerOpts ++ pkgs
  where
    pkgs = display <$> Set.toList targets
    constraints = [ constraint (display (PackageVersionConstraint pn vr))
                  | (pn, vr) <- toList (targetPackages packageSetSpec)
                  ]
    lowerBounds = [ constraint (unPackageName pn <> " >= " <> display v)
                  | (pn, v) <- toList (packageSet packageSetSpec)
                  ]
    installed = [ constraint (unPackageName pn <> " == " <> display v)
                | (pn, v) <- toList (corePackages packageSetSpec)
                ]
    flagOpts = [ constraint (unPackageName pn <> " " <> display fa) | (pn, fa) <- toList (flags packageSetSpec) ]
    allowNewerOpts = [ "--allow-newer=" <> unPackageName x <> ":" <> unPackageName y
                     | (x, ys) <- toList (allowNewer packageSetSpec), y <- Set.toList ys
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
  plan <- resolve solve (Map.keysSet (targetPackages packageSetSpec)) Map.empty
  putStrLn (render (ppPackageSet (plan `Map.withoutKeys` Map.keysSet (corePackages packageSetSpec))))

-- | Iterate the solver until the plan is stable.

resolve :: (PackageNameSet -> IO String) -> PackageNameSet -> PackageSet -> IO PackageSet
resolve solve targets previous = do
  plan <- parseInstallPlan <$> solve targets
  if plan == previous
     then pure plan
     else resolve solve (Map.keysSet (targetPackages packageSetSpec) `Set.union` Map.keysSet plan) plan

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

-- | Instantiate all 'pkgConfigDependencies' from nixpkgs and return a search
-- path for pkg-config that covers their @.pc@ files.

pkgConfigPath :: IO String
pkgConfigPath = do
  paths <- nixBuild ("with import <nixpkgs> {}; map lib.getDev [ " <> unwords (Set.toList (pkgConfigDependencies packageSetSpec)) <> " ]")
  pure $ intercalate ":" [ p <> d | p <- paths, d <- ["/lib/pkgconfig", "/share/pkgconfig"] ]

-- | Build a nix expression and return the store paths of its outputs.

nixBuild :: String -> IO [FilePath]
nixBuild expr = do
  Stdout out <- cmd Nix.nixBuild (["--no-out-link", "-E", expr] :: [String])
  pure (lines out)
