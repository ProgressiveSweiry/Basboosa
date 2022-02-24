{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_electrum_mnemonic (
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
version = Version [0,1,3] []

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/home/papa/.cabal/store/ghc-8.10.7/electrum-mnemonic-0.1.3-7dc9363184df8ecc08ee6ce6e1650eca4ad91073af820c7b3d3eff91e917cd0d/bin"
libdir     = "/home/papa/.cabal/store/ghc-8.10.7/electrum-mnemonic-0.1.3-7dc9363184df8ecc08ee6ce6e1650eca4ad91073af820c7b3d3eff91e917cd0d/lib"
dynlibdir  = "/home/papa/.cabal/store/ghc-8.10.7/electrum-mnemonic-0.1.3-7dc9363184df8ecc08ee6ce6e1650eca4ad91073af820c7b3d3eff91e917cd0d/lib"
datadir    = "/home/papa/.cabal/store/ghc-8.10.7/electrum-mnemonic-0.1.3-7dc9363184df8ecc08ee6ce6e1650eca4ad91073af820c7b3d3eff91e917cd0d/share"
libexecdir = "/home/papa/.cabal/store/ghc-8.10.7/electrum-mnemonic-0.1.3-7dc9363184df8ecc08ee6ce6e1650eca4ad91073af820c7b3d3eff91e917cd0d/libexec"
sysconfdir = "/home/papa/.cabal/store/ghc-8.10.7/electrum-mnemonic-0.1.3-7dc9363184df8ecc08ee6ce6e1650eca4ad91073af820c7b3d3eff91e917cd0d/etc"

getBinDir     = catchIO (getEnv "electrum_mnemonic_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "electrum_mnemonic_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "electrum_mnemonic_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "electrum_mnemonic_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "electrum_mnemonic_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "electrum_mnemonic_sysconfdir") (\_ -> return sysconfdir)




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
