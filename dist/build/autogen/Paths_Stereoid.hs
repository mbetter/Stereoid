module Paths_Stereoid (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import Data.Version (Version(..))
import System.Environment (getEnv)

version :: Version
version = Version {versionBranch = [0,1], versionTags = []}

bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/root/.cabal/bin"
libdir     = "/root/.cabal/lib/Stereoid-0.1/ghc-7.0.3"
datadir    = "/root/.cabal/share/Stereoid-0.1"
libexecdir = "/root/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catch (getEnv "Stereoid_bindir") (\_ -> return bindir)
getLibDir = catch (getEnv "Stereoid_libdir") (\_ -> return libdir)
getDataDir = catch (getEnv "Stereoid_datadir") (\_ -> return datadir)
getLibexecDir = catch (getEnv "Stereoid_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
