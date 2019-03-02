{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_deBruijn (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

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

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/quentin/Documents/Epitech/Tek2/FUN/FUN_deBruijn_2018/.stack-work/install/x86_64-linux-tinfo6/lts-12.18/8.4.4/bin"
libdir     = "/home/quentin/Documents/Epitech/Tek2/FUN/FUN_deBruijn_2018/.stack-work/install/x86_64-linux-tinfo6/lts-12.18/8.4.4/lib/x86_64-linux-ghc-8.4.4/deBruijn-0.1.0.0-CWEFEfGPKRcG93Hp9g7mm8-deBruijn-exe"
dynlibdir  = "/home/quentin/Documents/Epitech/Tek2/FUN/FUN_deBruijn_2018/.stack-work/install/x86_64-linux-tinfo6/lts-12.18/8.4.4/lib/x86_64-linux-ghc-8.4.4"
datadir    = "/home/quentin/Documents/Epitech/Tek2/FUN/FUN_deBruijn_2018/.stack-work/install/x86_64-linux-tinfo6/lts-12.18/8.4.4/share/x86_64-linux-ghc-8.4.4/deBruijn-0.1.0.0"
libexecdir = "/home/quentin/Documents/Epitech/Tek2/FUN/FUN_deBruijn_2018/.stack-work/install/x86_64-linux-tinfo6/lts-12.18/8.4.4/libexec/x86_64-linux-ghc-8.4.4/deBruijn-0.1.0.0"
sysconfdir = "/home/quentin/Documents/Epitech/Tek2/FUN/FUN_deBruijn_2018/.stack-work/install/x86_64-linux-tinfo6/lts-12.18/8.4.4/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "deBruijn_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "deBruijn_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "deBruijn_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "deBruijn_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "deBruijn_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "deBruijn_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
