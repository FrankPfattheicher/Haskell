{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_Chapter45 (
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

bindir     = "C:\\ICT Baden\\Haskell\\Chapter45\\.stack-work\\install\\3573363d\\bin"
libdir     = "C:\\ICT Baden\\Haskell\\Chapter45\\.stack-work\\install\\3573363d\\lib\\x86_64-windows-ghc-8.0.2\\Chapter45-0.1.0.0-H5Dob5RLOcLFme2K1MSYyh"
dynlibdir  = "C:\\ICT Baden\\Haskell\\Chapter45\\.stack-work\\install\\3573363d\\lib\\x86_64-windows-ghc-8.0.2"
datadir    = "C:\\ICT Baden\\Haskell\\Chapter45\\.stack-work\\install\\3573363d\\share\\x86_64-windows-ghc-8.0.2\\Chapter45-0.1.0.0"
libexecdir = "C:\\ICT Baden\\Haskell\\Chapter45\\.stack-work\\install\\3573363d\\libexec"
sysconfdir = "C:\\ICT Baden\\Haskell\\Chapter45\\.stack-work\\install\\3573363d\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Chapter45_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Chapter45_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Chapter45_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Chapter45_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Chapter45_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Chapter45_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
