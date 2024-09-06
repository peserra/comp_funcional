{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_projeto2 (
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
bindir     = "/mnt/c/Users/pedro/OneDrive/code/haskell/projeto2/.stack-work/install/x86_64-linux/92c36805fe9f6c50550d3c2a8a248656d196867bb352f4e006c2315f46a90f33/9.6.6/bin"
libdir     = "/mnt/c/Users/pedro/OneDrive/code/haskell/projeto2/.stack-work/install/x86_64-linux/92c36805fe9f6c50550d3c2a8a248656d196867bb352f4e006c2315f46a90f33/9.6.6/lib/x86_64-linux-ghc-9.6.6/projeto2-0.1.0.0-FMSmSGOTxu2DDNq8I65BIi-projeto2"
dynlibdir  = "/mnt/c/Users/pedro/OneDrive/code/haskell/projeto2/.stack-work/install/x86_64-linux/92c36805fe9f6c50550d3c2a8a248656d196867bb352f4e006c2315f46a90f33/9.6.6/lib/x86_64-linux-ghc-9.6.6"
datadir    = "/mnt/c/Users/pedro/OneDrive/code/haskell/projeto2/.stack-work/install/x86_64-linux/92c36805fe9f6c50550d3c2a8a248656d196867bb352f4e006c2315f46a90f33/9.6.6/share/x86_64-linux-ghc-9.6.6/projeto2-0.1.0.0"
libexecdir = "/mnt/c/Users/pedro/OneDrive/code/haskell/projeto2/.stack-work/install/x86_64-linux/92c36805fe9f6c50550d3c2a8a248656d196867bb352f4e006c2315f46a90f33/9.6.6/libexec/x86_64-linux-ghc-9.6.6/projeto2-0.1.0.0"
sysconfdir = "/mnt/c/Users/pedro/OneDrive/code/haskell/projeto2/.stack-work/install/x86_64-linux/92c36805fe9f6c50550d3c2a8a248656d196867bb352f4e006c2315f46a90f33/9.6.6/etc"

getBinDir     = catchIO (getEnv "projeto2_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "projeto2_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "projeto2_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "projeto2_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "projeto2_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "projeto2_sysconfdir") (\_ -> return sysconfdir)



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
