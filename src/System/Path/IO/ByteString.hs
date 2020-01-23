{-# LANGUAGE Safe #-}
-- | Wrappers around "Data.ByteString"
module System.Path.IO.ByteString
  (
    readLazyByteString
  , readStrictByteString
  , writeLazyByteString
  , writeStrictByteString
  , appendLazyByteString
  , appendStrictByteString
  ) where

import           System.Path

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as BS.L

-- | Wrapper around lazy 'BS.L.readFile'
readLazyByteString :: FsRoot root => Path root -> IO BS.L.ByteString
readLazyByteString path = do
    filePath <- toAbsoluteFilePath path
    BS.L.readFile filePath

-- | Wrapper around strict 'BS.readFile'
readStrictByteString :: FsRoot root => Path root -> IO BS.ByteString
readStrictByteString path = do
    filePath <- toAbsoluteFilePath path
    BS.readFile filePath

-- | Wrapper around lazy 'BS.L.writeFile'
writeLazyByteString :: FsRoot root => Path root -> BS.L.ByteString -> IO ()
writeLazyByteString path bs = do
    filePath <- toAbsoluteFilePath path
    BS.L.writeFile filePath bs

-- | Wrapper around strict 'BS.writeFile'
writeStrictByteString :: FsRoot root => Path root -> BS.ByteString -> IO ()
writeStrictByteString path bs = do
    filePath <- toAbsoluteFilePath path
    BS.writeFile filePath bs

-- | Wrapper around lazy 'BS.L.appendFile'
appendLazyByteString :: FsRoot root => Path root -> BS.L.ByteString -> IO ()
appendLazyByteString path bs = do
    filePath <- toAbsoluteFilePath path
    BS.L.appendFile filePath bs

-- | Wrapper around strict 'BS.appendFile'
appendStrictByteString :: FsRoot root => Path root -> BS.ByteString -> IO ()
appendStrictByteString path bs = do
    filePath <- toAbsoluteFilePath path
    BS.appendFile filePath bs
