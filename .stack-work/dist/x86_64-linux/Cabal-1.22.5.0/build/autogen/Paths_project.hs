module Paths_project (
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

bindir     = "/home/mahmad/Studies/sem8/ProgLang/project/project-release/.stack-work/install/x86_64-linux/5e6de43e70002e55c04c3373e0fce3a9816934c1f74928a2a74ff453e993adfb/7.10.3/bin"
libdir     = "/home/mahmad/Studies/sem8/ProgLang/project/project-release/.stack-work/install/x86_64-linux/5e6de43e70002e55c04c3373e0fce3a9816934c1f74928a2a74ff453e993adfb/7.10.3/lib/x86_64-linux-ghc-7.10.3/project-0.1.0.0-LLHqKK2hNU8CGpP0faBy6m"
datadir    = "/home/mahmad/Studies/sem8/ProgLang/project/project-release/.stack-work/install/x86_64-linux/5e6de43e70002e55c04c3373e0fce3a9816934c1f74928a2a74ff453e993adfb/7.10.3/share/x86_64-linux-ghc-7.10.3/project-0.1.0.0"
libexecdir = "/home/mahmad/Studies/sem8/ProgLang/project/project-release/.stack-work/install/x86_64-linux/5e6de43e70002e55c04c3373e0fce3a9816934c1f74928a2a74ff453e993adfb/7.10.3/libexec"
sysconfdir = "/home/mahmad/Studies/sem8/ProgLang/project/project-release/.stack-work/install/x86_64-linux/5e6de43e70002e55c04c3373e0fce3a9816934c1f74928a2a74ff453e993adfb/7.10.3/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "project_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "project_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "project_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "project_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "project_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
