module Paths_grid (
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
version = Version [1,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/x86_64-linux-ghc-7.10.1/grid_I4DZM1cgOi7J7gl9upbAiZ"
datadir    = "/usr/local/share/x86_64-linux-ghc-7.10.1/grid-1.0"
libexecdir = "/usr/local/libexec"
sysconfdir = "/usr/local/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "grid_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "grid_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "grid_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "grid_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "grid_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
