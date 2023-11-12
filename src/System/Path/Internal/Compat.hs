{-# LANGUAGE CPP  #-}
{-# LANGUAGE Trustworthy #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- compat layer
module System.Path.Internal.Compat
    ( Applicative(..)
    , (<$>)
    , dirMakeAbsolute
    , posixIsExtensionOf
    , XdgDirectory (..)
    , getXdgDirectory
    ) where

import           Control.Applicative
import           Data.List             (isSuffixOf)
import qualified System.Directory      as Dir
import qualified System.Environment    as Env
import qualified System.FilePath       as FP.Native
import qualified System.FilePath.Posix as FP.Posix

#ifdef mingw32_HOST_OS
import qualified System.Win32          as Win32
import           Foreign.Ptr           (nullPtr)
#endif

dirMakeAbsolute :: FilePath -> IO FilePath
#if MIN_VERSION_directory(1,2,2)
dirMakeAbsolute = Dir.makeAbsolute
#else
-- copied implementation from the directory package
dirMakeAbsolute = (FP.Native.normalise <$>) . absolutize
  where
    absolutize path -- avoid the call to `getCurrentDirectory` if we can
      | FP.Native.isRelative path
                  = (FP.Native.</> path)
                  . FP.Native.addTrailingPathSeparator <$>
                    Dir.getCurrentDirectory
      | otherwise = return path
#endif


posixIsExtensionOf :: String -> FilePath -> Bool
#if MIN_VERSION_filepath(1,4,2)
posixIsExtensionOf = FP.Posix.isExtensionOf
#else
posixIsExtensionOf ext@('.':_) = isSuffixOf ext . FP.Posix.takeExtensions
posixIsExtensionOf ext         = isSuffixOf ('.':ext) . FP.Posix.takeExtensions
#endif

-------------------------------------------------------------------------------
-- We vendor in XDG related code, so it's uniform independently
-- of directory version.
-------------------------------------------------------------------------------

data XdgDirectory = XdgData | XdgConfig | XdgCache

getXdgDirectory :: XdgDirectory -> FilePath -> IO FilePath
getXdgDirectory xdgDir suffix = do
    (FP.Native.</> suffix) <$> do
      env <- lookupEnv $ case xdgDir of
        XdgData   -> "XDG_DATA_HOME"
        XdgConfig -> "XDG_CONFIG_HOME"
        XdgCache  -> "XDG_CACHE_HOME"
      case env of
        Nothing   -> getXdgDirectoryFallback Dir.getHomeDirectory xdgDir
        Just path
            | FP.Native.isRelative path -> getXdgDirectoryFallback Dir.getHomeDirectory xdgDir
            | otherwise                 -> return path

getXdgDirectoryFallback :: IO FilePath -> XdgDirectory -> IO FilePath
#ifdef mingw32_HOST_OS
getXdgDirectoryFallback _ xdgDir = do
  case xdgDir of
    XdgData   -> getFolderPath Win32.cSIDL_APPDATA
    XdgConfig -> getFolderPath Win32.cSIDL_APPDATA
-- this is wrong, but affects only GHC-7.6.3
#if MIN_VERSION_Win32(2,3,1)
    XdgCache  -> getFolderPath Win32.cSIDL_LOCAL_APPDATA
#else
    XdgCache  -> getFolderPath Win32.cSIDL_APPDATA
#endif

getFolderPath :: Win32.CSIDL -> IO FilePath
getFolderPath what = Win32.sHGetFolderPath nullPtr what nullPtr 0
#else
getXdgDirectoryFallback getHomeDirectory xdgDir = do
  (<$> getHomeDirectory) $ flip (FP.Native.</>) $ case xdgDir of
    XdgData   -> ".local/share"
    XdgConfig -> ".config"
    XdgCache  -> ".cache"
#endif

-- | Return the value of the environment variable @var@, or @Nothing@ if
-- there is no such value.
--
-- For POSIX users, this is equivalent to 'System.Posix.Env.getEnv'.
lookupEnv :: String -> IO (Maybe String)
#if MIN_VERSION_base(4,6,0)
lookupEnv = Env.lookupEnv
#else
lookupEnv k = lookup k `fmap` Env.getEnvironment
#endif
