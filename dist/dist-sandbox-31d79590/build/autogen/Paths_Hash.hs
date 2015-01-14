module Paths_Hash (
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
version = Version {versionBranch = [1,0,0,0], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/karlo/Programiranje/Haskell/haskell-fer-project/.cabal-sandbox/bin"
libdir     = "/home/karlo/Programiranje/Haskell/haskell-fer-project/.cabal-sandbox/lib/x86_64-linux-ghc-7.6.3/Hash-1.0.0.0"
datadir    = "/home/karlo/Programiranje/Haskell/haskell-fer-project/.cabal-sandbox/share/x86_64-linux-ghc-7.6.3/Hash-1.0.0.0"
libexecdir = "/home/karlo/Programiranje/Haskell/haskell-fer-project/.cabal-sandbox/libexec"
sysconfdir = "/home/karlo/Programiranje/Haskell/haskell-fer-project/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Hash_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Hash_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Hash_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Hash_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Hash_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
