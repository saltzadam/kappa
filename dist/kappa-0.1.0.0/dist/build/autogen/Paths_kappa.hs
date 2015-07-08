module Paths_kappa (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\Adam\\AppData\\Roaming\\cabal\\bin"
libdir     = "C:\\Users\\Adam\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-7.8.3\\kappa-0.1.0.0"
datadir    = "C:\\Users\\Adam\\AppData\\Roaming\\cabal\\x86_64-windows-ghc-7.8.3\\kappa-0.1.0.0"
libexecdir = "C:\\Users\\Adam\\AppData\\Roaming\\cabal\\kappa-0.1.0.0"
sysconfdir = "C:\\Users\\Adam\\AppData\\Roaming\\cabal\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "kappa_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "kappa_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "kappa_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "kappa_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "kappa_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
