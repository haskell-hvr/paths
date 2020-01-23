{-# LANGUAGE CPP  #-}
{-# LANGUAGE Safe #-}
module System.Path.IO.Handle
  (
    -- * Wrappers around "System.IO"
    withFile
  , openTempFile'

    -- * Re-exports
  , IOMode(..)
  , BufferMode(..)
  , Handle
  , SeekMode(..)
  , IO.hSetBuffering
  , IO.hClose
  , IO.hFileSize
  , IO.hSeek
  ) where

import           System.Path

import           System.IO                (BufferMode (..), Handle, IOMode (..),
                                           SeekMode (..))
import qualified System.IO                as IO

{-------------------------------------------------------------------------------
  Wrappers around System.IO
-------------------------------------------------------------------------------}

-- | Wrapper around 'withFile'
withFile :: FsRoot root => Path root -> IOMode -> (Handle -> IO r) -> IO r
withFile path mode callback = do
    filePath <- toAbsoluteFilePath path
    IO.withFile filePath mode callback

-- | Wrapper around 'openBinaryTempFileWithDefaultPermissions'
--
-- NOTE: The caller is responsible for cleaning up the temporary file.
openTempFile' :: FsRoot root => Path root -> String -> IO (Path Absolute, Handle)
openTempFile' path template = do
    filePath <- toAbsoluteFilePath path
    (tempFilePath, h) <- IO.openBinaryTempFileWithDefaultPermissions filePath template
    return (fromAbsoluteFilePath tempFilePath, h)

