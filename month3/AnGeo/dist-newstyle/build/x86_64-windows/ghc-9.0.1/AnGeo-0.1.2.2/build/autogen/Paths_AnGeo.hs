{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -Wno-missing-safe-haskell-mode #-}
module Paths_AnGeo (
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
version = Version [0,1,2,2] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Murav\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\Murav\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-9.0.1\\AnGeo-0.1.2.2-inplace"
dynlibdir  = "C:\\Users\\Murav\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-9.0.1"
datadir    = "C:\\Users\\Murav\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-9.0.1\\AnGeo-0.1.2.2"
libexecdir = "C:\\Users\\Murav\\AppData\\Roaming\\cabal\\AnGeo-0.1.2.2-inplace\\x86_64-windows-ghc-9.0.1\\AnGeo-0.1.2.2"
sysconfdir = "C:\\Users\\Murav\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "AnGeo_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "AnGeo_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "AnGeo_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "AnGeo_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "AnGeo_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "AnGeo_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
