module Paths_tutorial5 (
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
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/baber/baber/hku/semester04/principals-of-pl/TA/project/project/.stack-work/install/x86_64-linux/lts-6.17/7.10.3/bin"
libdir     = "/home/baber/baber/hku/semester04/principals-of-pl/TA/project/project/.stack-work/install/x86_64-linux/lts-6.17/7.10.3/lib/x86_64-linux-ghc-7.10.3/tutorial5-0.1.0.0-0OyEuVATxN33kR1yQT748r"
datadir    = "/home/baber/baber/hku/semester04/principals-of-pl/TA/project/project/.stack-work/install/x86_64-linux/lts-6.17/7.10.3/share/x86_64-linux-ghc-7.10.3/tutorial5-0.1.0.0"
libexecdir = "/home/baber/baber/hku/semester04/principals-of-pl/TA/project/project/.stack-work/install/x86_64-linux/lts-6.17/7.10.3/libexec"
sysconfdir = "/home/baber/baber/hku/semester04/principals-of-pl/TA/project/project/.stack-work/install/x86_64-linux/lts-6.17/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "tutorial5_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "tutorial5_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "tutorial5_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "tutorial5_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "tutorial5_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
