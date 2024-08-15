{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_playground (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/mnt/c/Users/pedro/OneDrive/code/haskell/playground/.stack-work/install/x86_64-linux/2c29d5233062c432603791c1f8323a955ef1cd9ce1107cca3b2165ce95c789d3/9.6.6/bin"
libdir     = "/mnt/c/Users/pedro/OneDrive/code/haskell/playground/.stack-work/install/x86_64-linux/2c29d5233062c432603791c1f8323a955ef1cd9ce1107cca3b2165ce95c789d3/9.6.6/lib/x86_64-linux-ghc-9.6.6/playground-0.1.0.0-Lcov7p8JRBt4A38uVMwVpR-playground"
dynlibdir  = "/mnt/c/Users/pedro/OneDrive/code/haskell/playground/.stack-work/install/x86_64-linux/2c29d5233062c432603791c1f8323a955ef1cd9ce1107cca3b2165ce95c789d3/9.6.6/lib/x86_64-linux-ghc-9.6.6"
datadir    = "/mnt/c/Users/pedro/OneDrive/code/haskell/playground/.stack-work/install/x86_64-linux/2c29d5233062c432603791c1f8323a955ef1cd9ce1107cca3b2165ce95c789d3/9.6.6/share/x86_64-linux-ghc-9.6.6/playground-0.1.0.0"
libexecdir = "/mnt/c/Users/pedro/OneDrive/code/haskell/playground/.stack-work/install/x86_64-linux/2c29d5233062c432603791c1f8323a955ef1cd9ce1107cca3b2165ce95c789d3/9.6.6/libexec/x86_64-linux-ghc-9.6.6/playground-0.1.0.0"
sysconfdir = "/mnt/c/Users/pedro/OneDrive/code/haskell/playground/.stack-work/install/x86_64-linux/2c29d5233062c432603791c1f8323a955ef1cd9ce1107cca3b2165ce95c789d3/9.6.6/etc"

getBinDir     = catchIO (getEnv "playground_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "playground_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "playground_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "playground_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "playground_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "playground_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
