module Paths_ghc_events_hdph (
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
version = Version {versionBranch = [0,4,2,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/mmaa10/.cabal/bin"
libdir     = "/home/mmaa10/.cabal/lib/x86_64-linux-ghc-7.6.3/ghc-events-hdph-0.4.2.0"
datadir    = "/home/mmaa10/.cabal/share/x86_64-linux-ghc-7.6.3/ghc-events-hdph-0.4.2.0"
libexecdir = "/home/mmaa10/.cabal/libexec"
sysconfdir = "/home/mmaa10/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ghc_events_hdph_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ghc_events_hdph_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "ghc_events_hdph_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ghc_events_hdph_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ghc_events_hdph_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
