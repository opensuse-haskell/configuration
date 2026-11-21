{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Resolve the target packages of a package set with @cabal install
-- --dry-run@ and update the package set specification accordingly. cabal2obs
-- uses this for its @update@ target.

module UpdatePackageSet ( cabalInstallResolver, PackageSetSpec(..), updateTargetPackages, writeBackPackageSetUpdates ) where

import Config ( PackageSetSpec(..), writeBackPackageSetUpdates, updateTargetPackages )
import MyCabal
import Types

import Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Data.Map as Map
import Data.Map.Strict ( toList )
import qualified Data.Set as Set
import Development.Shake.Command ( CmdOption(AddEnv), Stdout(..), cmd )
import Data.List ( intercalate )
import qualified Nix.Paths as Nix

-- | Resolve the target packages of the given package set with @cabal install
-- --dry-run@ and return the resulting package set, without the core packages
-- of the compiler.
--
-- A plan computed by @cabal install@ contains only the libraries of all
-- dependencies, but we build every package completely, including executables
-- and test suites. So we re-run the solver with all packages of the previous
-- plan as targets until the plan no longer changes.

cabalInstallResolver :: (MonadIO m, MonadFail m) => PackageSetSpec -> m PackageSet
cabalInstallResolver spec = do
  ghc <- getCompilerPath (compiler spec)
  pkgConfig <- nixpkgsPkgconfigSearchPath spec
  let solve targets = liftIO $ do
        Stdout out <- cmd (AddEnv "PKG_CONFIG_PATH" pkgConfig) (askCabalInstall spec ghc targets)
        pure (parseInstallPlan out)
  plan <- iterateResolver spec solve (Map.keysSet (targetPackages spec)) Map.empty
  pure (plan `Map.withoutKeys` Map.keysSet (corePackages spec))

-- | Iterate the resolver, adding discovered dependencies to the target set,
-- until the plan becomes stable.

iterateResolver :: Monad m => PackageSetSpec -> (PackageNameSet -> m PackageSet) -> PackageNameSet -> PackageSet -> m PackageSet
iterateResolver spec solve targets previous = do
  plan <- solve targets
  if plan == previous
     then pure plan
     else iterateResolver spec solve (Map.keysSet (targetPackages spec) `Set.union` Map.keysSet plan) plan

-- | Generate a @cabal install --dry-run@ command that resolves the given target
-- packages under the constraints of our package set.

askCabalInstall :: PackageSetSpec -> CompilerPath -> PackageNameSet -> [String]
askCabalInstall spec ghc targets = ["cabal", "--store=/var/empty", "install", "--ignore-project", "--lib", "--dry-run", "--with-compiler=" <> ghc, "--minimize-conflict-set", "--upgrade-dependencies"]
                                  ++ installed ++ lowerBounds ++ constraints ++ flagOpts ++ allowNewerOpts ++ pkgs
  where
    pkgs = display <$> Set.toList targets
    constraints = [ constraint (display (PackageVersionConstraint pn vr))
                  | (pn, vr) <- toList (targetPackages spec)
                  ]
    lowerBounds = [ constraint (unPackageName pn <> " >= " <> display v)
                  | (pn, v) <- toList (packageSet spec)
                  ]
    installed = [ constraint (unPackageName pn <> " == " <> display v)
                | (pn, v) <- toList (corePackages spec)
                ]
    flagOpts = [ constraint (unPackageName pn <> " " <> display fa) | (pn, fa) <- toList (flags spec) ]
    allowNewerOpts = [ "--allow-newer=" <> unPackageName x <> ":" <> unPackageName y
                     | (x, ys) <- toList (allowNewer spec), y <- Set.toList ys
                     ]
    constraint c = "--constraint=any." <> c

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

-- | Instantiate the given compiler from nixpkgs and return the path of its
-- @ghc@ binary.

getCompilerPath :: (MonadIO m, MonadFail m) => CompilerId -> m CompilerPath
getCompilerPath cid = do
  ghc : _ <- liftIO (nixBuild ("(import <nixpkgs> {})." <> nixpkgsCompiler cid))
  pure (ghc <> "/bin/ghc")

-- | The nixpkgs attribute of the given compiler. It collapses the version
-- number into the attribute name:
--
-- >>> :set -XOverloadedStrings
-- >>> nixpkgsCompiler "ghc-9.14.1"
-- "haskell.compiler.ghc9141"

nixpkgsCompiler :: CompilerId -> NixpkgsAttribute
nixpkgsCompiler (CompilerId flavor v) = "haskell.compiler." <> display flavor <> concatMap show (versionNumbers v)

-- | Instantiate all 'pkgConfigDependencies' from nixpkgs and return a search
-- path for pkg-config that covers their @.pc@ files.

nixpkgsPkgconfigSearchPath :: MonadIO m => PackageSetSpec -> m String
nixpkgsPkgconfigSearchPath spec = do
  paths <- liftIO $ nixBuild ("with import <nixpkgs> {}; map lib.getDev [ " <> unwords (Set.toList (pkgConfigDependencies spec)) <> " ]")
  pure $ intercalate ":" [ p <> d | p <- paths, d <- ["/lib/pkgconfig", "/share/pkgconfig"] ]

-- | Build a nix expression and return the store paths of its outputs.

nixBuild :: String -> IO [FilePath]
nixBuild expr = do
  Stdout out <- cmd Nix.nixBuild (["--no-out-link", "-E", expr] :: [String])
  pure (lines out)
