{-# LANGUAGE CPP         #-}
{-# LANGUAGE Trustworthy #-}
-- | Wrappers around "System.Directory"
module System.Path.IO.Directory
  (
    copyFile
  , createDirectory
  , createDirectoryIfMissing
  , removeDirectory
  , doesFileExist
  , doesDirectoryExist
  , getModificationTime
  , removeFile
  , getTemporaryDirectory
  , listDirectory
  , getRecursiveContents
  , renameFile
  , getCurrentDirectory
  ) where

import           System.Path

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative      ((<$>))
#endif

import           Control.Monad
import           Data.Time                (UTCTime)
import           System.IO.Unsafe         (unsafeInterleaveIO)

#if !MIN_VERSION_directory(1,2,0)
import           Data.Time.Clock          (picosecondsToDiffTime)
import           Data.Time.Clock.POSIX    (posixSecondsToUTCTime)
import           System.Time              (ClockTime (TOD))
#endif

import qualified System.Directory         as Dir

copyFile :: (FsRoot root, FsRoot root') => Path root -> Path root' -> IO ()
copyFile src dst = do
    src' <- toAbsoluteFilePath src
    dst' <- toAbsoluteFilePath dst
    Dir.copyFile src' dst'

createDirectory :: FsRoot root => Path root -> IO ()
createDirectory path = Dir.createDirectory =<< toAbsoluteFilePath path

createDirectoryIfMissing :: FsRoot root => Bool -> Path root -> IO ()
createDirectoryIfMissing createParents path = do
    filePath <- toAbsoluteFilePath path
    Dir.createDirectoryIfMissing createParents filePath

removeDirectory :: FsRoot root => Path root -> IO ()
removeDirectory path = Dir.removeDirectory =<< toAbsoluteFilePath path

doesFileExist :: FsRoot root => Path root -> IO Bool
doesFileExist path = do
    filePath <- toAbsoluteFilePath path
    Dir.doesFileExist filePath

doesDirectoryExist :: FsRoot root => Path root -> IO Bool
doesDirectoryExist path = do
    filePath <- toAbsoluteFilePath path
    Dir.doesDirectoryExist filePath

getModificationTime :: FsRoot root => Path root -> IO UTCTime
getModificationTime path = do
    filePath <- toAbsoluteFilePath path
    toUTC <$> Dir.getModificationTime filePath
  where
#if MIN_VERSION_directory(1,2,0)
    toUTC :: UTCTime -> UTCTime
    toUTC = id
#else
    toUTC :: ClockTime -> UTCTime
    toUTC (TOD secs psecs) = posixSecondsToUTCTime $ realToFrac $ picosecondsToDiffTime (psecs + secs*1000000000000)
#endif

removeFile :: FsRoot root => Path root -> IO ()
removeFile path = do
    filePath <- toAbsoluteFilePath path
    Dir.removeFile filePath

getTemporaryDirectory :: IO (Path Absolute)
getTemporaryDirectory = fromAbsoluteFilePath <$> Dir.getTemporaryDirectory

-- | Return the immediate children of a directory
--
-- Filters out @"."@ and @".."@.
listDirectory :: FsRoot root => Path root -> IO [Path Unrooted]
listDirectory path = do
    filePath <- toAbsoluteFilePath path
    fragments' <$> Dir.getDirectoryContents filePath
  where
    fragments' :: [String] -> [Path Unrooted]
    fragments' = map fragment . filter (not . skip)

    skip :: String -> Bool
    skip "."  = True
    skip ".." = True
    skip _    = False

-- | Recursive traverse a directory structure
--
-- Returns a set of paths relative to the directory specified. The list is
-- lazily constructed, so that directories are only read when required.
-- (This is also essential to ensure that this function does not build the
-- entire result in memory before returning, potentially running out of heap.)
getRecursiveContents :: FsRoot root => Path root -> IO [Path Unrooted]
getRecursiveContents root = go emptyPath
  where
    go :: Path Unrooted -> IO [Path Unrooted]
    go subdir = unsafeInterleaveIO $ do
      entries <- listDirectory (root </> subdir)
      liftM concat $ forM entries $ \entry -> do
        let path = subdir </> entry
        isDirectory <- doesDirectoryExist (root </> path)
        if isDirectory then go path
                       else return [path]

    emptyPath :: Path Unrooted
    emptyPath = joinFragments []

renameFile :: (FsRoot root, FsRoot root')
           => Path root  -- ^ Old
           -> Path root' -- ^ New
           -> IO ()
renameFile old new = do
    old' <- toAbsoluteFilePath old
    new' <- toAbsoluteFilePath new
    Dir.renameFile old' new'

getCurrentDirectory :: IO (Path Absolute)
getCurrentDirectory = do
    cwd <- Dir.getCurrentDirectory
    makeAbsolute $ fromFilePath cwd

