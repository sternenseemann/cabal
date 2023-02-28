-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Simple.Build.Macros
-- Copyright   :  Isaac Jones 2003-2005,
--                Ross Paterson 2006,
--                Duncan Coutts 2007-2008
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Generating the Paths_pkgname module.
--
-- This is a module that Cabal generates for the benefit of packages. It
-- enables them to find their version number and find any installed data files
-- at runtime. This code should probably be split off into another module.
module Distribution.Simple.Build.PathsModule
  ( generatePathsModule
  , pkgPathEnvVar
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Package
import Distribution.PackageDescription
import Distribution.Simple.Compiler
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Utils (shortRelativePath)
import Distribution.System
import Distribution.Version

import qualified Distribution.Simple.Build.PathsModule.Z as Z

-- ------------------------------------------------------------

-- * Building Paths_<pkg>.hs

-- ------------------------------------------------------------

generatePathsModule :: PackageDescription -> LocalBuildInfo -> ComponentLocalBuildInfo -> String
generatePathsModule pkg_descr lbi clbi =
  Z.render
    Z.Z
      { Z.zPackageName = packageName pkg_descr
      , Z.zVersionDigits = show $ versionNumbers $ packageVersion pkg_descr
      , Z.zSupportsCpp = supports_cpp
      , Z.zSupportsNoRebindableSyntax = supports_rebindable_syntax
      , Z.zAbsolute = absolute
      , Z.zRelocatable = relocatable lbi
      , Z.zIsWindows = isWindows
      , Z.zIsI386 = buildArch == I386
      , Z.zIsX8664 = buildArch == X86_64
      , Z.zOr = (||)
      , Z.zNot = not
      , Z.zManglePkgName = showPkgName
      , Z.zPrefix = show flat_prefix
      , Z.zBindir = zBindir
      , Z.zLibdir = zLibdir
      , Z.zDynlibdir = zDynlibdir
      , Z.zDatadir = zDatadir
      , Z.zLibexecdir = zLibexecdir
      , Z.zSysconfdir = zSysconfdir
      , -- Sadly we can't be cleverer about this – we can't have literals in the template
        Z.zShouldEmitDataDir = shouldEmit "DataDir"
      , Z.zShouldEmitLibDir = shouldEmit "LibDir"
      , Z.zShouldEmitDynLibDir = shouldEmit "DynLibDir"
      , Z.zShouldEmitLibexecDir = shouldEmit "LibexecDir"
      , Z.zShouldEmitSysconfDir = shouldEmit "SysconfDir"
      , Z.zWarning = zWarning
      , Z.zShouldEmitWarning = zShouldEmitWarning
      }
  where
    -- GHC's NCG backend for aarch64-darwin does not support link-time dead code
    -- elimination to the extent that NCG does for other targets. Consequently,
    -- we struggle with unnecessarily retained store path references due to the
    -- use of `Paths_*` modules – even if `getLibDir` is not used, it'll end up
    -- in the final library or executables we build.
    --
    -- When using a different output for the executables and library, this
    -- becomes more sinister: The library will contain a reference to the bin
    -- output and itself due to `getLibDir` and `getBinDir`, but the executables
    -- will do so, too. Either due to linking dynamically or because the library
    -- is linked statically into the executable and retains those references.
    -- Since Nix disallows cyclical references between two outputs, it becomes
    -- impossible to use the `Paths_*` module and a separate `bin` output for
    -- aarch64-darwin.
    --
    -- The solution we have resorted to for now, is to trim the `Paths_*` module
    -- dynamically depending on what references *could* be used without causing
    -- a cyclical reference. That has the effect that any code that would not
    -- cause a cyclical reference with dead code elimination will compile and
    -- work for aarch64-darwin. If the code would use a `get*Dir` function that
    -- has been omitted, this would indicate that the code would have caused a
    -- cyclical reference anyways.
    --
    -- The logic for this makes some pretty big assumptions about installation
    -- prefixes that probably only hold fully in nixpkgs with
    -- `haskellPackages.mkDerivation`. Simple uses outside nixpkgs that have
    -- everything below the same prefix should continue to work as expected,
    -- though.
    --
    -- We assume the following:
    --
    -- - flat_prefix is `$out`.
    -- - flat_libdir etc. are always below `$out`.
    --
    -- Since in the normal case due to static linking `$bin` and `$out` will
    -- have the same references in libraries/executables, we need to either
    -- prevent usage of `getBinDir` or `getLibDir` to break the cycle in case
    -- `flat_bindir` is not below `$out`. We have decided to always allow usage
    -- of `getBinDir`, so `getLibDir` gets dropped if a separate `bin` output is
    -- used. This has the simple reason that `$out` which contains `flat_libdir`
    -- tends to be quite big – we would like to have a `bin` output that doesn't
    -- require keeping that around.
    pathEmittable :: FilePath -> Bool
    pathEmittable p
      -- If the executable installation target is below `$out` the reference
      -- cycle is within a single output (since libs are installed to `$out`)
      -- and thus unproblematic. We can use any and all `get*Dir` functions.
      | flat_prefix `isPrefixOf` flat_bindir = True
      -- Otherwise, we need to disallow all `get*Dir` functions that would cause
      -- a reference to `$out` which contains the libraries that would in turn
      -- reference `$bin`. This always include `flat_libdir` and friends, but
      -- can also include `flat_datadir` if no separate output for data files is
      -- used.
      | otherwise = not (flat_prefix `isPrefixOf` p)

    -- This list maps the "name" of the directory to whether we want to include
    -- it in the `Paths_*` module or not. `shouldEmit` performs a lookup in this.
    dirs :: [(String, Bool)]
    dirs =
      map
        (\(name, path) -> (name, pathEmittable path))
        [ ("LibDir", flat_libdir)
        , ("DynLibDir", flat_dynlibdir)
        , ("DataDir", flat_datadir)
        , ("LibexecDir", flat_libexecdir)
        , ("SysconfDir", flat_sysconfdir)
        ]

    shouldEmit :: String -> Bool
    shouldEmit name =
      case lookup name dirs of
        Just b -> b
        Nothing -> error "panic! BUG in Cabal Paths_ patch for aarch64-darwin, report this at https://github.com/nixos/nixpkgs/issues"

    -- This is a comma separated list of all functions that have been emitted.
    -- This is included in a GHC warning which will be attached to the `Paths_*`
    -- module in case we are dropping any `get*Dir` functions that would
    -- normally exist.
    --
    -- TODO: getDataFileName is not accounted for at the moment.
    omittedFunctions :: String
    omittedFunctions =
      intercalate ", " $
        map (("get" ++) . fst) $
          filter (not . snd) dirs

    zWarning :: String
    zWarning =
      show $
        "The following functions have been omitted by a nixpkgs-specific patch to Cabal: "
          ++ omittedFunctions
    zShouldEmitWarning :: Bool
    zShouldEmitWarning = any (not . snd) dirs

    supports_cpp = supports_language_pragma
    supports_rebindable_syntax = ghc_newer_than (mkVersion [7, 0, 1])
    supports_language_pragma = ghc_newer_than (mkVersion [6, 6, 1])

    ghc_newer_than minVersion =
      case compilerCompatVersion GHC (compiler lbi) of
        Nothing -> False
        Just version -> version `withinRange` orLaterVersion minVersion

    -- In several cases we cannot make relocatable installations
    absolute =
      hasLibs pkg_descr -- we can only make progs relocatable
        || isNothing flat_bindirrel -- if the bin dir is an absolute path
        || not (supportsRelocatableProgs (compilerFlavor (compiler lbi)))

    -- TODO: Here, and with zIsI386 & zIs8664 we should use TARGET platform
    isWindows = case buildOS of
      Windows -> True
      _ -> False

    supportsRelocatableProgs GHC = isWindows
    supportsRelocatableProgs GHCJS = isWindows
    supportsRelocatableProgs _ = False

    cid = componentUnitId clbi

    InstallDirs
      { bindir = flat_bindir
      , libdir = flat_libdir
      , dynlibdir = flat_dynlibdir
      , datadir = flat_datadir
      , libexecdir = flat_libexecdir
      , sysconfdir = flat_sysconfdir
      , prefix = flat_prefix
      } = absoluteInstallCommandDirs pkg_descr lbi cid NoCopyDest

    InstallDirs
      { bindir = flat_bindirrel
      , libdir = flat_libdirrel
      , dynlibdir = flat_dynlibdirrel
      , datadir = flat_datadirrel
      , libexecdir = flat_libexecdirrel
      , sysconfdir = flat_sysconfdirrel
      } = prefixRelativeComponentInstallDirs (packageId pkg_descr) lbi cid

    zBindir, zLibdir, zDynlibdir, zDatadir, zLibexecdir, zSysconfdir :: String
    (zBindir, zLibdir, zDynlibdir, zDatadir, zLibexecdir, zSysconfdir)
      | relocatable lbi =
          ( show flat_bindir_reloc
          , show flat_libdir_reloc
          , show flat_dynlibdir_reloc
          , show flat_datadir_reloc
          , show flat_libexecdir_reloc
          , show flat_sysconfdir_reloc
          )
      | absolute =
          ( show flat_bindir
          , show flat_libdir
          , show flat_dynlibdir
          , show flat_datadir
          , show flat_libexecdir
          , show flat_sysconfdir
          )
      | isWindows =
          ( "maybe (error \"PathsModule.generate\") id (" ++ show flat_bindirrel ++ ")"
          , mkGetDir flat_libdir flat_libdirrel
          , mkGetDir flat_dynlibdir flat_dynlibdirrel
          , mkGetDir flat_datadir flat_datadirrel
          , mkGetDir flat_libexecdir flat_libexecdirrel
          , mkGetDir flat_sysconfdir flat_sysconfdirrel
          )
      | otherwise =
          error "panic! generatePathsModule: should never happen"

    mkGetDir :: FilePath -> Maybe FilePath -> String
    mkGetDir _ (Just dirrel) = "getPrefixDirRel " ++ show dirrel
    mkGetDir dir Nothing = "return " ++ show dir

    flat_bindir_reloc = shortRelativePath flat_prefix flat_bindir
    flat_libdir_reloc = shortRelativePath flat_prefix flat_libdir
    flat_dynlibdir_reloc = shortRelativePath flat_prefix flat_dynlibdir
    flat_datadir_reloc = shortRelativePath flat_prefix flat_datadir
    flat_libexecdir_reloc = shortRelativePath flat_prefix flat_libexecdir
    flat_sysconfdir_reloc = shortRelativePath flat_prefix flat_sysconfdir

-- | Generates the name of the environment variable controlling the path
-- component of interest.
--
-- Note: The format of these strings is part of Cabal's public API;
-- changing this function constitutes a *backwards-compatibility* break.
pkgPathEnvVar
  :: PackageDescription
  -> String
  -- ^ path component; one of \"bindir\", \"libdir\", -- \"datadir\", \"libexecdir\", or \"sysconfdir\"
  -> String
  -- ^ environment variable name
pkgPathEnvVar pkg_descr var =
  showPkgName (packageName pkg_descr) ++ "_" ++ var

showPkgName :: PackageName -> String
showPkgName = map fixchar . unPackageName

fixchar :: Char -> Char
fixchar '-' = '_'
fixchar c = c
