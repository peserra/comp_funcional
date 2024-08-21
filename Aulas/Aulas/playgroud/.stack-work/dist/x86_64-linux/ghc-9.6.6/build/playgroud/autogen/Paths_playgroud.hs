{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_playgroud (
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
bindir     = "/home/ufabc/Aulas/playgroud/.stack-work/install/x86_64-linux/b3e2f9b9dce0cd0f80415e3b9d81906ace09131fd76907bed9ac1be8830f3892/9.6.6/bin"
libdir     = "/home/ufabc/Aulas/playgroud/.stack-work/install/x86_64-linux/b3e2f9b9dce0cd0f80415e3b9d81906ace09131fd76907bed9ac1be8830f3892/9.6.6/lib/x86_64-linux-ghc-9.6.6/playgroud-0.1.0.0-J9qUNXxqbDC5ZblRxUTuI2-playgroud"
dynlibdir  = "/home/ufabc/Aulas/playgroud/.stack-work/install/x86_64-linux/b3e2f9b9dce0cd0f80415e3b9d81906ace09131fd76907bed9ac1be8830f3892/9.6.6/lib/x86_64-linux-ghc-9.6.6"
datadir    = "/home/ufabc/Aulas/playgroud/.stack-work/install/x86_64-linux/b3e2f9b9dce0cd0f80415e3b9d81906ace09131fd76907bed9ac1be8830f3892/9.6.6/share/x86_64-linux-ghc-9.6.6/playgroud-0.1.0.0"
libexecdir = "/home/ufabc/Aulas/playgroud/.stack-work/install/x86_64-linux/b3e2f9b9dce0cd0f80415e3b9d81906ace09131fd76907bed9ac1be8830f3892/9.6.6/libexec/x86_64-linux-ghc-9.6.6/playgroud-0.1.0.0"
sysconfdir = "/home/ufabc/Aulas/playgroud/.stack-work/install/x86_64-linux/b3e2f9b9dce0cd0f80415e3b9d81906ace09131fd76907bed9ac1be8830f3892/9.6.6/etc"

getBinDir     = catchIO (getEnv "playgroud_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "playgroud_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "playgroud_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "playgroud_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "playgroud_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "playgroud_sysconfdir") (\_ -> return sysconfdir)



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
