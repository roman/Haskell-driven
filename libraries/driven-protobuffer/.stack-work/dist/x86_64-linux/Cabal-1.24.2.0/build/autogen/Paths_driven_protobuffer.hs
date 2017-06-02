{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_driven_protobuffer (
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

bindir     = "/home/ubuntu/driven/.stack-work/install/x86_64-linux/lts-8.16/8.0.2/bin"
libdir     = "/home/ubuntu/driven/.stack-work/install/x86_64-linux/lts-8.16/8.0.2/lib/x86_64-linux-ghc-8.0.2/driven-protobuffer-0.1.0.0-3n6HOqxqPbu4DW8MtfWblU"
dynlibdir  = "/home/ubuntu/driven/.stack-work/install/x86_64-linux/lts-8.16/8.0.2/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/ubuntu/driven/.stack-work/install/x86_64-linux/lts-8.16/8.0.2/share/x86_64-linux-ghc-8.0.2/driven-protobuffer-0.1.0.0"
libexecdir = "/home/ubuntu/driven/.stack-work/install/x86_64-linux/lts-8.16/8.0.2/libexec"
sysconfdir = "/home/ubuntu/driven/.stack-work/install/x86_64-linux/lts-8.16/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "driven_protobuffer_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "driven_protobuffer_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "driven_protobuffer_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "driven_protobuffer_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "driven_protobuffer_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "driven_protobuffer_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
