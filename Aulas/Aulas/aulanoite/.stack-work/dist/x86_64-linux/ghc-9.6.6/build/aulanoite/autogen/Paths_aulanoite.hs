{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_aulanoite (
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
bindir     = "/home/ufabc/Aulas/aulanoite/.stack-work/install/x86_64-linux/95d3adf29c7e3a2d99eda8ca11f16b0d46ba3aa781d4689ca138750181ddd620/9.6.6/bin"
libdir     = "/home/ufabc/Aulas/aulanoite/.stack-work/install/x86_64-linux/95d3adf29c7e3a2d99eda8ca11f16b0d46ba3aa781d4689ca138750181ddd620/9.6.6/lib/x86_64-linux-ghc-9.6.6/aulanoite-0.1.0.0-31I1MTNpR5z7i3zC9PAiXN-aulanoite"
dynlibdir  = "/home/ufabc/Aulas/aulanoite/.stack-work/install/x86_64-linux/95d3adf29c7e3a2d99eda8ca11f16b0d46ba3aa781d4689ca138750181ddd620/9.6.6/lib/x86_64-linux-ghc-9.6.6"
datadir    = "/home/ufabc/Aulas/aulanoite/.stack-work/install/x86_64-linux/95d3adf29c7e3a2d99eda8ca11f16b0d46ba3aa781d4689ca138750181ddd620/9.6.6/share/x86_64-linux-ghc-9.6.6/aulanoite-0.1.0.0"
libexecdir = "/home/ufabc/Aulas/aulanoite/.stack-work/install/x86_64-linux/95d3adf29c7e3a2d99eda8ca11f16b0d46ba3aa781d4689ca138750181ddd620/9.6.6/libexec/x86_64-linux-ghc-9.6.6/aulanoite-0.1.0.0"
sysconfdir = "/home/ufabc/Aulas/aulanoite/.stack-work/install/x86_64-linux/95d3adf29c7e3a2d99eda8ca11f16b0d46ba3aa781d4689ca138750181ddd620/9.6.6/etc"

getBinDir     = catchIO (getEnv "aulanoite_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "aulanoite_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "aulanoite_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "aulanoite_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "aulanoite_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "aulanoite_sysconfdir") (\_ -> return sysconfdir)



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
