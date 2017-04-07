module Cabal2Spec ( createSpecFile ) where

import Control.Monad
import Data.Char
import Data.List
import Data.Time.Clock
import Data.Time.Format
import Data.Version
import Distribution.License
import Distribution.Package
import Distribution.PackageDescription
import System.Directory
import System.FilePath
import System.IO

type ForceBinary = Bool

showPkgCfg :: String -> String
showPkgCfg p = "pkgconfig(" ++ p ++ ")"

mkTools :: [String] -> [String]
mkTools tools' = filter excludedTools $ nub $ map mapTools tools'
  where
    excludedTools n = n `notElem` ["ghc", "hsc2hs", "perl"]
    mapTools "gtk2hsC2hs" = "gtk2hs-buildtools"
    mapTools "gtk2hsHookGenerator" = "gtk2hs-buildtools"
    mapTools "gtk2hsTypeGen" = "gtk2hs-buildtools"
    mapTools tool = tool

createSpecFile :: FilePath -> FilePath -> PackageDescription -> ForceBinary -> FlagAssignment -> IO ()
createSpecFile specFile cabalPath pkgDesc forceBinary flagAssignment = do
  let deps :: [String]
      deps = map showDep deps'
      deps' :: [String]
      selfdep :: Bool
      (deps', selfdep) = buildDependencies pkgDesc name
      pkgcfgs :: [String]
      pkgcfgs = map showPkgCfg (nub $ map depName $ concatMap pkgconfigDepends buildinfo)
      buildinfo :: [BuildInfo]
      buildinfo = allBuildInfo pkgDesc
      tools :: [String]
      tools = mkTools (nub $ map depName (concatMap buildTools buildinfo)) ++ chrpath
      clibs :: [String]
      clibs = nub (map resolveLib (concatMap extraLibs buildinfo))
      chrpath :: [String]
      chrpath = ["chrpath" | selfdep]

      pkg = package pkgDesc
      PackageName name = packageName pkg
      hasExec = hasExes pkgDesc
      hasLib = hasLibs pkgDesc
  (pkgname, binlib) <- getPkgName (Just specFile) pkgDesc forceBinary

  let pkg_name = if pkgname == name then "%{name}" else "%{pkg_name}"
      basename | binlib = "%{pkg_name}"
               | hasExecPkg = name
               | otherwise = "ghc-%{pkg_name}"

      hasExecPkg = binlib || (hasExec && not hasLib)
  -- run commands before opening file to prevent empty file on error
  -- maybe shell commands should be in a monad or something

      testsuiteDeps = testsuiteDependencies pkgDesc name

  h <- openFile specFile WriteMode
  let putHdr hdr val = hPutStrLn h (hdr ++ ":" ++ padding hdr ++ val)
      padding hdr = replicate (14 - length hdr) ' ' ++ " "
      putNewline = hPutStrLn h ""
      put = hPutStrLn h
      putDef v s = put $ "%global" +-+ v +-+ s
      ghcPkg = if binlib then "-n ghc-%{name}" else ""
      ghcPkgDevel = if binlib then "-n ghc-%{name}-devel" else "devel"

  do
    now <- getCurrentTime
    let year = formatTime defaultTimeLocale "%Y" now
    put "#"
    put $ "# spec file for package " ++ pkgname
    put "#"
    put $ "# Copyright (c) " ++ year ++ " SUSE LINUX GmbH, Nuernberg, Germany."
    put "#"
    put "# All modifications and additions to the file contributed by third parties"
    put "# remain the property of their copyright owners, unless otherwise agreed"
    put "# upon. The license for this file, and modifications and additions to the"
    put "# file, is the same license as for the pristine package itself (unless the"
    put "# license for the pristine package is not an Open Source License, in which"
    put "# case the license is the MIT License). An \"Open Source License\" is a"
    put "# license that conforms to the Open Source Definition (Version 1.9)"
    put "# published by the Open Source Initiative."
    putNewline
    put "# Please submit bugfixes or comments via http://bugs.opensuse.org/"
    put "#"
  putNewline

  -- Some packages conflate the synopsis and description fields.  Ugh.
  let syn = synopsis pkgDesc
  let initialCapital (c:cs) = toUpper c:cs
      initialCapital [] = []
  let syn' = if badDescription syn then "FIXME" else (unwords . lines . initialCapital) syn
  let summary = rstrip (== '.') syn'
  let descr = description pkgDesc
  let descLines = (formatParagraphs . initialCapital . filterSymbols . finalPeriod) $ if badDescription descr then syn' else descr
      finalPeriod cs = if last cs == '.' then cs else cs ++ "."
      filterSymbols (c:cs) =
        if c `notElem` "@\\" then c: filterSymbols cs
        else case c of
          '@' -> '\'': filterSymbols cs
          '\\' -> head cs: filterSymbols (tail cs)
          _ -> c: filterSymbols cs
      filterSymbols [] = []
  when hasLib $ do
    putDef "pkg_name" name
    putNewline

  unless (null testsuiteDeps) $ do
    put "%bcond_with tests"
    putNewline

  let version = packageVersion pkg
      revision = show $ maybe (0::Int) read (lookup "x-revision" (customFieldsPD pkgDesc))
  putHdr "Name" (if binlib then "%{pkg_name}" else basename)
  putHdr "Version" (showVersion version)
  putHdr "Release" "0"
  putHdr "Summary" summary
  putHdr "Group" "Development/Languages/Other"
  putNewline
  putHdr "License" $ showLicense (license pkgDesc)
  putHdr "Url" $ "https://hackage.haskell.org/package/" ++ pkg_name
  putHdr "Source0" $ "https://hackage.haskell.org/package/" ++ pkg_name ++ "-%{version}/" ++ pkg_name ++ "-%{version}.tar.gz"
  when (revision /= "0") $
    putHdr "Source1" $ "https://hackage.haskell.org/package/" ++ pkg_name ++ "-%{version}/revision/" ++ revision ++ ".cabal#/" ++ pkg_name ++ ".cabal"
  putHdr "BuildRoot" "%{_tmppath}/%{name}-%{version}-build"
  putNewline
  putHdr "BuildRequires" "ghc-Cabal-devel"
  putHdr "BuildRequires" "ghc-rpm-macros"

  let isa = ""
  let alldeps = sort $ deps ++ tools ++ map (++ isa) clibs ++ pkgcfgs
  let extraTestDeps = sort $ testsuiteDeps \\ deps
  unless (null $ alldeps ++ extraTestDeps) $ do
    mapM_ (putHdr "BuildRequires") alldeps
    unless (null extraTestDeps) $ do
      put "%if %{with tests}"
      mapM_ (putHdr "BuildRequires") extraTestDeps
      put "%endif"

  putNewline

  put "%description"
  mapM_ put descLines
  putNewline

  let wrapGenDesc = wordwrap (79 - max 0 (length pkgname - length pkg_name))

  when hasLib $ do
    when binlib $ do
      put $ "%package" +-+ ghcPkg
      putHdr "Summary" $ "Haskell" +-+ pkg_name +-+ "library"
      putHdr "Group" "System/Libraries"
      putNewline
      put $ "%description" +-+ ghcPkg
      put $ wrapGenDesc $ "This package provides the Haskell" +-+ pkg_name +-+ "shared library."
      putNewline
    put $ "%package" +-+ ghcPkgDevel
    putHdr "Summary" $ "Haskell" +-+ pkg_name +-+ "library development files"
    putHdr "Group" "Development/Libraries/Other"
    putHdr "Requires" "ghc-compiler = %{ghc_version}"
    putHdr "Requires(post)" "ghc-compiler = %{ghc_version}"
    putHdr "Requires(postun)" "ghc-compiler = %{ghc_version}"
    putHdr "Requires" $ (if binlib then "ghc-%{name}" else "%{name}") ++ isa +-+ "= %{version}-%{release}"
    unless (null $ clibs ++ pkgcfgs) $
      mapM_ (putHdr "Requires") $ sort $ map (++ isa) clibs ++ pkgcfgs ++ ["pkgconfig" | not $ null pkgcfgs]
    putNewline
    put $ "%description" +-+ ghcPkgDevel
    put $ wrapGenDesc $ "This package provides the Haskell" +-+ pkg_name +-+ "library development files."
    putNewline

  put "%prep"
  put $ "%setup -q" ++ (if pkgname /= name then " -n %{pkg_name}-%{version}" else "")
  when (revision /= "0") $
    put $ "cp -p %{SOURCE1}" +-+ pkg_name ++ ".cabal"
  putNewline

  put "%build"
  when (flagAssignment /= []) $ do
    let cabalFlags = [ "-f" ++ (if b then "" else "-") ++ n | (FlagName n, b) <- flagAssignment ]
    put $ "%define cabal_configure_options " ++ unwords cabalFlags
  let pkgType = if hasLib then "lib" else "bin"
  put $ "%ghc_" ++ pkgType ++ "_build"
  putNewline

  put "%install"
  put $ "%ghc_" ++ pkgType ++ "_install"

  when selfdep $
    put $ "%ghc_fix_rpath" +-+ "%{pkg_name}-%{version}"

  let licensefiles = licenseFiles pkgDesc

  -- remove docs from datafiles (#38)
  docsUnfiltered <- sort <$> findDocs cabalPath licensefiles
  let datafiles = dataFiles pkgDesc
      dupdocs   = docsUnfiltered `intersect` datafiles
      docs      = docsUnfiltered \\ datafiles
  unless (null dupdocs) $
    putStrLn $ "Warning: doc files found in datadir:" +-+ unwords dupdocs
  putNewline

  unless (null testsuiteDeps) $ do
    put "%check"
    put "%cabal_test"
    putNewline

  when hasLib $ do
    let putInstallScript = do
          put "%ghc_pkg_recache"
          putNewline
    put $ "%post" +-+ ghcPkgDevel
    putInstallScript
    put $ "%postun" +-+ ghcPkgDevel
    putInstallScript

  let license_macro = "%doc"
  let execs = sort $ map exeName $ filter isBuildable $ executables pkgDesc

  let listDataFiles = do unless (null (dataFiles pkgDesc)) $ do
                           put ("%dir %{_datadir}/" ++ pkg_name ++ "-%{version}")
                           mapM_ (put . (("%dir %{_datadir}/" ++ pkg_name ++ "-%{version}/")++)) (listDirs (dataFiles pkgDesc))
                         mapM_ (put . (("%{_datadir}/" ++ pkg_name ++ "-%{version}/")++)) (dataFiles pkgDesc)

      listDirs :: [FilePath] -> [FilePath]
      listDirs = nub . concatMap (map joinPath . tail . inits) . nub . map init . filter (\p -> length p > 1) . map splitDirectories

  when hasExecPkg $ do
    put "%files"
    put "%defattr(-,root,root,-)"
    -- Add the license file to the main package only if it wouldn't
    -- otherwise be empty.
    mapM_ (\ l -> put $ license_macro +-+ l) licensefiles
    unless (null docs) $
      put $ "%doc" +-+ unwords docs
    mapM_ (\ p -> put $ "%{_bindir}/" ++ (if p == name then "%{name}" else p)) execs
    listDataFiles
    putNewline

  when hasLib $ do
    let baseFiles = if binlib then "-f ghc-%{name}.files" else "-f %{name}.files"
        develFiles = if binlib then "-f ghc-%{name}-devel.files" else "-f %{name}-devel.files"
    put $ "%files" +-+ ghcPkg +-+ baseFiles
    put "%defattr(-,root,root,-)"
    mapM_ (\ l -> put $ license_macro +-+ l) licensefiles
    unless binlib $
      mapM_ (\ p -> put $ "%{_bindir}/" ++ (if p == name then "%{pkg_name}" else p)) execs
    unless hasExecPkg listDataFiles
    putNewline
    put $ "%files" +-+ ghcPkgDevel +-+  develFiles
    put "%defattr(-,root,root,-)"
    unless (null docs) $
      put $ "%doc" +-+ unwords docs
    putNewline

  put "%changelog"
  hClose h


isBuildable :: Executable -> Bool
isBuildable exe = buildable $ buildInfo exe

findDocs :: FilePath -> [FilePath] -> IO [FilePath]
findDocs cabalPath licensefiles = do
  contents <- getDirectoryContents $ dropFileName cabalPath
  let docs = filter likely contents
  return $ if null licensefiles
           then docs
           else filter unlikely $ filter (`notElem` licensefiles) docs
  where names = ["author", "changelog", "changes", "contributors", "copying", "doc",
                 "example", "licence", "license", "news", "readme", "todo"]
        likely name = let lowerName = map toLower name
                      in any (`isPrefixOf` lowerName) names
        unlikely name = not $ any (`isSuffixOf` name) ["~"]

normalizeVersion :: Version -> Version
normalizeVersion (Version [v] vt) = Version [v,0] vt
normalizeVersion v = v

showLicense :: License -> String
showLicense (GPL Nothing) = "GPL-1.0+"
showLicense (GPL (Just ver)) = "GPL-" ++ showVersion (normalizeVersion ver) ++ "+"
showLicense (LGPL Nothing) = "LGPL-2.0+"
showLicense (LGPL (Just ver)) = "LGPL-" ++ showVersion (normalizeVersion ver) ++ "+"
showLicense BSD3 = "BSD-3-Clause"
showLicense BSD4 = "BSD-4-Clause"
showLicense MIT = "MIT"
showLicense PublicDomain = "SUSE-Public-Domain"
showLicense AllRightsReserved = "SUSE-NonFree"
showLicense OtherLicense = "Unknown"
showLicense (UnknownLicense l) = "Unknown" +-+ l
showLicense (Apache Nothing) = "Apache-2.0"
showLicense (Apache (Just ver)) = "Apache-" ++ showVersion (normalizeVersion ver)
showLicense (AGPL Nothing) = "AGPLv?"
showLicense (AGPL (Just ver)) = "AGPLv" ++ showVersion ver
showLicense BSD2 = "BSD-2-Clause"
showLicense (MPL ver) = "MPL-" ++ showVersion (normalizeVersion ver)
showLicense ISC = "ISC"
showLicense UnspecifiedLicense = "Unspecified license!"

-- http://rosettacode.org/wiki/Word_wrap#Haskell
wordwrap :: Int -> String -> String
wordwrap maxlen = wrap_ 0 False . words
  where
    wrap_ _ _ [] = "\n"
    wrap_ pos eos (w:ws)
      -- at line start: put down the word no matter what
      | pos == 0 = w ++ wrap_ (pos + lw) endp ws
      | pos + lw + 1 > maxlen - 9 && eos = '\n':wrap_ 0 endp (w:ws)
      | pos + lw + 1 > maxlen = '\n':wrap_ 0 endp (w:ws)
      | otherwise = " " ++ w ++ wrap_ (pos + lw + 1) endp ws
      where
        lw = length w
        endp = last w == '.'

formatParagraphs :: String -> [String]
formatParagraphs = map (wordwrap 79) . paragraphs . lines
  where
    -- from http://stackoverflow.com/questions/930675/functional-paragraphs
    -- using split would be: map unlines . (Data.List.Split.splitWhen null)
    paragraphs :: [String] -> [String]
    paragraphs = map (unlines . filter (not . null)) . groupBy (const $ not . null)

rstrip :: (Char -> Bool) -> String -> String
rstrip p = reverse . dropWhile p . reverse

getPkgName :: Maybe FilePath -> PackageDescription -> Bool -> IO (String, Bool)
getPkgName (Just spec) pkgDesc binary = do
  let PackageName name = packageName $ package pkgDesc
      pkgname = takeBaseName spec
      hasLib = hasLibs pkgDesc
  return $ if name == pkgname || binary then (name, hasLib) else (pkgname, False)
getPkgName Nothing pkgDesc binary = do
  let PackageName name = packageName $ package pkgDesc
      hasExec = hasExes pkgDesc
      hasLib = hasLibs pkgDesc
  return $ if binary || hasExec && not hasLib then (name, hasLib) else ("ghc-" ++ name, False)

infixr 4 +-+
(+-+) :: String -> String -> String
"" +-+ s = s
s +-+ "" = s
s +-+ t = s ++ " " ++ t

excludedPkgs :: String -> Bool
excludedPkgs = flip notElem ["Cabal", "base", "ghc-prim", "integer-gmp"]

-- returns list of deps and whether package is self-dependent
buildDependencies :: PackageDescription -> String -> ([String], Bool)
buildDependencies pkgDesc self =
  let bdeps = map depName (buildDepends pkgDesc)
      sdeps = maybe [] (map depName . setupDepends) (setupBuildInfo pkgDesc)
      deps  = nub $ bdeps ++ sdeps
  in
    (filter excludedPkgs (delete self deps), self `elem` deps && hasExes pkgDesc)

depName :: Dependency -> String
depName (Dependency (PackageName n) _) = n

showDep :: String -> String
showDep p = "ghc-" ++ p ++ "-devel"

resolveLib :: String -> String
resolveLib "adns" = "libadns-devel"
resolveLib "alut" = "freealut-devel"
resolveLib "asound" = "alsa-devel"
resolveLib "blas" = "blas-devel"
resolveLib "bz2" = "libbz2-devel"
resolveLib "clang" = "clang-devel"
resolveLib "crypt" = "glibc-devel"
resolveLib "crypto" = "libopenssl-devel"
resolveLib "curl" = "libcurl-devel"
resolveLib "expat" = "libexpat-devel"
resolveLib "ffi" = "libffi-devel"
resolveLib "fftw3" = "fftw3-devel"
resolveLib "FLAC" = "flac-devel"
resolveLib "fontconfig" = "fontconfig-devel"
resolveLib "freetype" = "freetype2-devel"
resolveLib "gd" = "gd-devel"
resolveLib "GeoIP" = "libGeoIP-devel"
resolveLib "GL" = "Mesa-libGL-devel"
resolveLib "GLU" = "glu-devel"
resolveLib "gmp" = "gmp-devel"
resolveLib "gnutls" = "libgnutls-devel"
resolveLib "icudata" = "libicu-devel"
resolveLib "icui18n" = "libicu-devel"
resolveLib "icuuc" = "libicu-devel"
resolveLib "idn" = "libidn-devel"
resolveLib "IL" = "DevIL-devel"
resolveLib "iw" = "libiw-devel"
resolveLib "jpeg" = "libjpeg62-devel"
resolveLib "lapack" = "lapack-devel"
resolveLib "leveldb" = "leveldb-devel"
resolveLib "lmdb" = "lmdb-devel"
resolveLib "lua" = "lua-devel"
resolveLib "lzma" = "xz-devel"
resolveLib "magic" = "file-devel"
resolveLib "markdown" = "libmarkdown-devel"
resolveLib "m" = "glibc-devel"
resolveLib "mpfr" = "mpfr-devel"
resolveLib "nfc" = "libnfc-devel"
resolveLib "notify" = "libnotify-devel"
resolveLib "openal" = "openal-soft-devel"
resolveLib "pcre" = "pcre-devel"
resolveLib "png" = "libpng16-compat-devel"
resolveLib "pq" = "postgresql94-devel"
resolveLib "pthread" = "glibc-devel"
resolveLib "resolv" = "glibc-devel"
resolveLib "ruby" = "ruby2.2-devel"
resolveLib "sass" = "libsass-devel"
resolveLib "SDL2" = "libSDL2-devel"
resolveLib "SDL2_mixer" = "libSDL2_mixer-devel"
resolveLib "SDL2_ttf" = "libSDL2_ttf-devel"
resolveLib "SDL_image" = "libSDL_image-devel"
resolveLib "SDL" = "libSDL-devel"
resolveLib "SDL_mixer" = "libSDL_mixer-devel"
resolveLib "sndfile" = "libsndfile-devel"
resolveLib "sqlite3" = "sqlite3-devel"
resolveLib "ssl" = "libopenssl-devel"
resolveLib "stdc++" = "libstdc++-devel"
resolveLib "tag_c" = "libtag-devel"
resolveLib "udev" = "libudev-devel"
resolveLib "X11" = "libX11-devel"
resolveLib "Xcursor" = "libXcursor-devel"
resolveLib "Xext" = "libXext-devel"
resolveLib "Xi" = "libXi-devel"
resolveLib "Xinerama" = "libXinerama-devel"
resolveLib "xml2" = "libxml2-devel"
resolveLib "Xpm" = "libXpm-devel"
resolveLib "Xrandr" = "libXrandr-devel"
resolveLib "Xrender" = "libXrender-devel"
resolveLib "Xss" = "libXss-devel"
resolveLib "Xxf86vm" = "libXxf86vm-devel"
resolveLib "z" = "zlib-devel"
resolveLib s = error ("resolveLib: cannot map library name " ++ show s ++ " to package name")

testsuiteDependencies :: PackageDescription  -- ^pkg description
                -> String           -- ^self
                -> [String]         -- ^depends
testsuiteDependencies pkgDesc self =
  map showDep . delete self . filter excludedPkgs . nub . map depName $ concatMap (targetBuildDepends . testBuildInfo) (testSuites pkgDesc)

badDescription :: String -> Bool
badDescription s = null s
                || "please see readme" `isPrefixOf` map toLower s
                || "please see the readme" `isPrefixOf` map toLower s
                || "see readme" `isPrefixOf` map toLower s
                || "cf readme" `isPrefixOf` map toLower s
                || "please refer to readme" `isPrefixOf` map toLower s
                || "initial project template" `isPrefixOf` map toLower s
