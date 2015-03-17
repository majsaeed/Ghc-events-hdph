module Paths_ghc_events (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,4,2,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/u1/pg/mmaa10/.cabal/bin"
libdir     = "/u1/pg/mmaa10/.cabal/lib/ghc-events-0.4.2.0/ghc-7.4.2"
datadir    = "/u1/pg/mmaa10/.cabal/share/ghc-events-0.4.2.0"
libexecdir = "/u1/pg/mmaa10/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "ghc_events_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ghc_events_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "ghc_events_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ghc_events_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
