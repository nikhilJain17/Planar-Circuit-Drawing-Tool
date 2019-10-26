{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_planar_circuit (
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

bindir     = "/Users/nikhiljain/college/edg-research/planar_circuit_outputter/Planar_Circuit_Drawings/planar-circuit/.stack-work/install/x86_64-osx/d5c9f23a7b2623e2f6b8868f4e295f821743728e0e86d3f2865f67e2d0ff9522/8.6.5/bin"
libdir     = "/Users/nikhiljain/college/edg-research/planar_circuit_outputter/Planar_Circuit_Drawings/planar-circuit/.stack-work/install/x86_64-osx/d5c9f23a7b2623e2f6b8868f4e295f821743728e0e86d3f2865f67e2d0ff9522/8.6.5/lib/x86_64-osx-ghc-8.6.5/planar-circuit-0.1.0.0-BbmvEdEuTyiITVk8DGPllz-planar-circuit"
dynlibdir  = "/Users/nikhiljain/college/edg-research/planar_circuit_outputter/Planar_Circuit_Drawings/planar-circuit/.stack-work/install/x86_64-osx/d5c9f23a7b2623e2f6b8868f4e295f821743728e0e86d3f2865f67e2d0ff9522/8.6.5/lib/x86_64-osx-ghc-8.6.5"
datadir    = "/Users/nikhiljain/college/edg-research/planar_circuit_outputter/Planar_Circuit_Drawings/planar-circuit/.stack-work/install/x86_64-osx/d5c9f23a7b2623e2f6b8868f4e295f821743728e0e86d3f2865f67e2d0ff9522/8.6.5/share/x86_64-osx-ghc-8.6.5/planar-circuit-0.1.0.0"
libexecdir = "/Users/nikhiljain/college/edg-research/planar_circuit_outputter/Planar_Circuit_Drawings/planar-circuit/.stack-work/install/x86_64-osx/d5c9f23a7b2623e2f6b8868f4e295f821743728e0e86d3f2865f67e2d0ff9522/8.6.5/libexec/x86_64-osx-ghc-8.6.5/planar-circuit-0.1.0.0"
sysconfdir = "/Users/nikhiljain/college/edg-research/planar_circuit_outputter/Planar_Circuit_Drawings/planar-circuit/.stack-work/install/x86_64-osx/d5c9f23a7b2623e2f6b8868f4e295f821743728e0e86d3f2865f67e2d0ff9522/8.6.5/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "planar_circuit_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "planar_circuit_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "planar_circuit_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "planar_circuit_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "planar_circuit_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "planar_circuit_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
