module Paths_gd (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [3000,4,0], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/usr/local/bin"
libdir     = "/usr/local/lib/gd-3000.4.0/ghc-6.10.1"
datadir    = "/usr/local/share/gd-3000.4.0"
libexecdir = "/usr/local/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "gd_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "gd_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "gd_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "gd_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
