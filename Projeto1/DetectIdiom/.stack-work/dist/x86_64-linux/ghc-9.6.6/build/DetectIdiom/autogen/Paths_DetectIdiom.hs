{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_DetectIdiom (
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
bindir     = "/mnt/Dados/code/haskell/Projeto1/DetectIdiom/.stack-work/install/x86_64-linux/60b5c7b6fdb66f6b9cf24decaf398695da4973c8d2c5cbda02a8ee710bb9c631/9.6.6/bin"
libdir     = "/mnt/Dados/code/haskell/Projeto1/DetectIdiom/.stack-work/install/x86_64-linux/60b5c7b6fdb66f6b9cf24decaf398695da4973c8d2c5cbda02a8ee710bb9c631/9.6.6/lib/x86_64-linux-ghc-9.6.6/DetectIdiom-0.1.0.0-19uh6x2wr0VIRLce4QMy9i-DetectIdiom"
dynlibdir  = "/mnt/Dados/code/haskell/Projeto1/DetectIdiom/.stack-work/install/x86_64-linux/60b5c7b6fdb66f6b9cf24decaf398695da4973c8d2c5cbda02a8ee710bb9c631/9.6.6/lib/x86_64-linux-ghc-9.6.6"
datadir    = "/mnt/Dados/code/haskell/Projeto1/DetectIdiom/.stack-work/install/x86_64-linux/60b5c7b6fdb66f6b9cf24decaf398695da4973c8d2c5cbda02a8ee710bb9c631/9.6.6/share/x86_64-linux-ghc-9.6.6/DetectIdiom-0.1.0.0"
libexecdir = "/mnt/Dados/code/haskell/Projeto1/DetectIdiom/.stack-work/install/x86_64-linux/60b5c7b6fdb66f6b9cf24decaf398695da4973c8d2c5cbda02a8ee710bb9c631/9.6.6/libexec/x86_64-linux-ghc-9.6.6/DetectIdiom-0.1.0.0"
sysconfdir = "/mnt/Dados/code/haskell/Projeto1/DetectIdiom/.stack-work/install/x86_64-linux/60b5c7b6fdb66f6b9cf24decaf398695da4973c8d2c5cbda02a8ee710bb9c631/9.6.6/etc"

getBinDir     = catchIO (getEnv "DetectIdiom_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "DetectIdiom_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "DetectIdiom_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "DetectIdiom_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "DetectIdiom_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "DetectIdiom_sysconfdir") (\_ -> return sysconfdir)



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
