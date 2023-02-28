{% if supportsCpp %}
{-# LANGUAGE CPP #-}
{% endif %}
{% if supportsNoRebindableSyntax %}
{-# LANGUAGE NoRebindableSyntax #-}
{% endif %}
{% if not absolute %}
{-# LANGUAGE ForeignFunctionInterface #-}
{% endif %}
{% if supportsCpp %}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{% endif %}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_{{ manglePkgName packageName }} (
    version,
    getBinDir,
{# We only care about the absolute case for our emit logic, since only in this
   case references are incurred. We are not going to hit isWindows and relocatable
   has no absolute references to begin with.
#}
{% if or (not absolute) shouldEmitLibDir %}
    getLibDir,
{% endif %}
{% if or (not absolute) shouldEmitDynLibDir %}
    getDynLibDir,
{% endif %}
{% if or (not absolute) shouldEmitLibexecDir %}
    getLibexecDir,
{% endif %}
{% if or (not absolute) shouldEmitDataDir %}
    getDataFileName,
    getDataDir,
{% endif %}
{% if or (not absolute) shouldEmitSysconfDir %}
    getSysconfDir
{% endif %}
  ) where

{% if not absolute %}
import Foreign
import Foreign.C
{% endif %}

import qualified Control.Exception as Exception
import qualified Data.List as List
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

{% if relocatable %}
import System.Environment (getExecutablePath)
{% endif %}

{% if supportsCpp %}
#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch
{% else %}
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch
{% endif %}

version :: Version
version = Version {{ versionDigits }} []

{% if or (not absolute) shouldEmitDataDir %}
getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)
{% endif %}

{% defblock function_defs %}
minusFileName :: FilePath -> String -> FilePath
minusFileName dir ""     = dir
minusFileName dir "."    = dir
minusFileName dir suffix =
  minusFileName (fst (splitFileName dir)) (fst (splitFileName suffix))

splitFileName :: FilePath -> (String, String)
splitFileName p = (reverse (path2++drive), reverse fname)
  where
    (path,drive) = case p of
      (c:':':p') -> (reverse p',[':',c])
      _          -> (reverse p ,"")
    (fname,path1) = break isPathSeparator path
    path2 = case path1 of
      []                           -> "."
      [_]                          -> path1   -- don't remove the trailing slash if
                                              -- there is only one character
      (c:path') | isPathSeparator c -> path'
      _                             -> path1
{% endblock %}

{# body #}
{# ######################################################################### #}

{% if relocatable %}

{# Relocatable can not incur any absolute references, so we can ignore it.
   Additionally, --enable-relocatable is virtually useless in Nix builds
#}

getPrefixDirReloc :: FilePath -> IO FilePath
getPrefixDirReloc dirRel = do
  exePath <- getExecutablePath
  let (dir,_) = splitFileName exePath
  return ((dir `minusFileName` {{ bindir }}) `joinFileName` dirRel)

getBinDir     :: IO FilePath
getBinDir     = catchIO (getEnv "{{ manglePkgName packageName }}_bindir")     (\_ -> getPrefixDirReloc $ {{ bindir }})
getLibDir     :: IO FilePath
getLibDir     = catchIO (getEnv "{{ manglePkgName packageName }}_libdir")     (\_ -> getPrefixDirReloc $ {{ libdir }})
getDynLibDir  :: IO FilePath
getDynLibDir  = catchIO (getEnv "{{ manglePkgName packageName }}_dynlibdir")  (\_ -> getPrefixDirReloc $ {{ dynlibdir }})
getDataDir    :: IO FilePath
getDataDir    = catchIO (getEnv "{{ manglePkgName packageName }}_datadir")    (\_ -> getPrefixDirReloc $ {{ datadir }})
getLibexecDir :: IO FilePath
getLibexecDir = catchIO (getEnv "{{ manglePkgName packageName }}_libexecdir") (\_ -> getPrefixDirReloc $ {{ libexecdir }})
getSysconfDir :: IO FilePath
getSysconfDir = catchIO (getEnv "{{ manglePkgName packageName }}_sysconfdir") (\_ -> getPrefixDirReloc $ {{ sysconfdir }})

{% useblock function_defs %}

{% elif absolute %}

bindir     :: FilePath
bindir     = {{ bindir }}
getBinDir     :: IO FilePath
getBinDir     = catchIO (getEnv "{{ manglePkgName packageName }}_bindir")     (\_ -> return bindir)

{% if shouldEmitLibDir %}
libdir     :: FilePath
libdir     = {{ libdir }}
getLibDir     :: IO FilePath
getLibDir     = catchIO (getEnv "{{ manglePkgName packageName }}_libdir")     (\_ -> return libdir)
{% endif %}

{% if shouldEmitDynLibDir %}
dynlibdir  :: FilePath
dynlibdir  = {{ dynlibdir }}
getDynLibDir  :: IO FilePath
getDynLibDir  = catchIO (getEnv "{{ manglePkgName packageName }}_dynlibdir")  (\_ -> return dynlibdir)
{% endif %}

{% if shouldEmitDataDir %}
datadir    :: FilePath
datadir    = {{ datadir }}
getDataDir    :: IO FilePath
getDataDir    = catchIO (getEnv "{{ manglePkgName packageName }}_datadir")    (\_ -> return datadir)
{% endif %}

{% if shouldEmitLibexecDir %}
libexecdir :: FilePath
libexecdir = {{ libexecdir }}
getLibexecDir :: IO FilePath
getLibexecDir = catchIO (getEnv "{{ manglePkgName packageName }}_libexecdir") (\_ -> return libexecdir)
{% endif %}

{% if shouldEmitSysconfDir %}
sysconfdir :: FilePath
sysconfdir = {{ sysconfdir }}
getSysconfDir :: IO FilePath
getSysconfDir = catchIO (getEnv "{{ manglePkgName packageName }}_sysconfdir") (\_ -> return sysconfdir)
{% endif %}

{% elif isWindows %}

{# We are only trying to fix the problem for aarch64-darwin with this patch,
   so let's ignore Windows which we can reach via pkgsCross, for example.
#}

prefix :: FilePath
prefix = {{ prefix }}

getBinDir     :: IO FilePath
getBinDir     = getPrefixDirRel $ {{ bindir }}
getLibDir     :: IO FilePath
getLibDir     = {{ libdir }}
getDynLibDir  :: IO FilePath
getDynLibDir  = {{ dynlibdir }}
getDataDir    :: IO FilePath
getDataDir    = catchIO (getEnv "{{ manglePkgName packageName }}_datadir")    (\_ -> {{ datadir }})
getLibexecDir :: IO FilePath
getLibexecDir = {{ libexecdir }}
getSysconfDir :: IO FilePath
getSysconfDir = {{ sysconfdir }}

getPrefixDirRel :: FilePath -> IO FilePath
getPrefixDirRel dirRel = try_size 2048 -- plenty, PATH_MAX is 512 under Win32.
  where
    try_size size = allocaArray (fromIntegral size) $ \buf -> do
        ret <- c_GetModuleFileName nullPtr buf size
        case ret of
          0 -> return (prefix `joinFileName` dirRel)
          _ | ret < size -> do
              exePath <- peekCWString buf
              let (bindir,_) = splitFileName exePath
              return ((bindir `minusFileName` {{ bindir}}) `joinFileName` dirRel)
            | otherwise  -> try_size (size * 2)

{% useblock function_defs %}

{% if isI386 %}
foreign import stdcall unsafe "windows.h GetModuleFileNameW"
  c_GetModuleFileName :: Ptr () -> CWString -> Int32 -> IO Int32
{% elif isX8664 %}
foreign import ccall unsafe "windows.h GetModuleFileNameW"
  c_GetModuleFileName :: Ptr () -> CWString -> Int32 -> IO Int32
{% else %}
-- win32 supported only with I386, X86_64
c_GetModuleFileName :: Ptr () -> CWString -> Int32 -> IO Int32
c_GetModuleFileName  = _
{% endif %}

{% else %}

notRelocAbsoluteOrWindows :: ()
notRelocAbsoluteOrWindows = _

{% endif %}

{# filename stuff                                                            #}
{# ######################################################################### #}

joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
{% if isWindows %}
pathSeparator = '\\'
{% else %}
pathSeparator = '/'
{% endif %}

isPathSeparator :: Char -> Bool
{% if isWindows %}
isPathSeparator c = c == '/' || c == '\\'
{% else %}
isPathSeparator c = c == '/'
{% endif %}
