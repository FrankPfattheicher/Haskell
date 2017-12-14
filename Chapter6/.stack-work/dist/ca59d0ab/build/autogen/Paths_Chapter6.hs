{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_Chapter6 (
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

bindir     = "C:\\ICT Baden\\Haskell\\Chapter6\\.stack-work\\install\\3e910dc5\\bin"
libdir     = "C:\\ICT Baden\\Haskell\\Chapter6\\.stack-work\\install\\3e910dc5\\lib\\x86_64-windows-ghc-8.0.2\\Chapter6-0.1.0.0-EFcCjIsD4I2CCofnzTMKRd"
dynlibdir  = "C:\\ICT Baden\\Haskell\\Chapter6\\.stack-work\\install\\3e910dc5\\lib\\x86_64-windows-ghc-8.0.2"
datadir    = "C:\\ICT Baden\\Haskell\\Chapter6\\.stack-work\\install\\3e910dc5\\share\\x86_64-windows-ghc-8.0.2\\Chapter6-0.1.0.0"
libexecdir = "C:\\ICT Baden\\Haskell\\Chapter6\\.stack-work\\install\\3e910dc5\\libexec"
sysconfdir = "C:\\ICT Baden\\Haskell\\Chapter6\\.stack-work\\install\\3e910dc5\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Chapter6_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Chapter6_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Chapter6_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Chapter6_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Chapter6_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Chapter6_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
