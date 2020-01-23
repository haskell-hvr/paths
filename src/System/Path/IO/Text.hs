{-# LANGUAGE CPP  #-}
{-# LANGUAGE Safe #-}
-- | Wrappers around "Data.Text.IO" and "Data.Text.Lazy.IO"
module System.Path.IO.Text
  (
    -- * Locale-dependent encoding
    readLazyText
  , readStrictText
  , writeLazyText
  , writeStrictText
  , appendLazyText
  , appendStrictText
    -- * [UTF-8](https://en.wikipedia.org/wiki/UTF-8) encoding
  , readLazyTextUtf8
  , readStrictTextUtf8
  , writeLazyTextUtf8
  , writeStrictTextUtf8
  , appendLazyTextUtf8
  , appendStrictTextUtf8
  ) where

import           System.Path
import           System.Path.IO.ByteString

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative      ((<$>))
#endif

import           Control.Exception        (evaluate)

import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T.E
import qualified Data.Text.IO             as T
import qualified Data.Text.Lazy           as T.L
import qualified Data.Text.Lazy.Encoding  as T.L.E
import qualified Data.Text.Lazy.IO        as T.L

#if defined(__HADDOCK_VERSION__)
import           Data.Text.Encoding.Error (UnicodeException)
#endif

--------------------------------------------------------------------------------
-- locale-dependent versions

-- | Wrapper around lazy 'T.L.readFile'
readLazyText :: FsRoot root => Path root -> IO T.L.Text
readLazyText path = do
    filePath <- toAbsoluteFilePath path
    T.L.readFile filePath

-- | Wrapper around strict 'T.readFile'
readStrictText :: FsRoot root => Path root -> IO T.Text
readStrictText path = do
    filePath <- toAbsoluteFilePath path
    T.readFile filePath

-- | Wrapper around lazy 'T.L.writeFile'
writeLazyText :: FsRoot root => Path root -> T.L.Text -> IO ()
writeLazyText path bs = do
    filePath <- toAbsoluteFilePath path
    T.L.writeFile filePath bs

-- | Wrapper around strict 'T.writeFile'
writeStrictText :: FsRoot root => Path root -> T.Text -> IO ()
writeStrictText path bs = do
    filePath <- toAbsoluteFilePath path
    T.writeFile filePath bs

-- | Wrapper around lazy 'T.L.appendFile'
appendLazyText :: FsRoot root => Path root -> T.L.Text -> IO ()
appendLazyText path bs = do
    filePath <- toAbsoluteFilePath path
    T.L.appendFile filePath bs

-- | Wrapper around strict 'T.appendFile'
appendStrictText :: FsRoot root => Path root -> T.Text -> IO ()
appendStrictText path bs = do
    filePath <- toAbsoluteFilePath path
    T.appendFile filePath bs

--------------------------------------------------------------------------------
-- UTF-8 versions

-- | Read lazy 'Text' from a file (using UTF-8 encoding).
--
-- __NOTE__: Since the file is read lazily UTF-8 decoding errors are detected lazily as well. Such errors will result in an 'Data.Text.Encoding.Error.UnicodeException' being thrown within the lazy 'Text' stream.
readLazyTextUtf8 :: FsRoot root => Path root -> IO T.L.Text
readLazyTextUtf8 path = T.L.E.decodeUtf8 <$> readLazyByteString path

-- | Read strict 'Text' from a file (using UTF-8 encoding).
--
-- __NOTE__: In case of UTF-8 decoding errors an 'Data.Text.Encoding.Error.UnicodeException' will be thrown.
readStrictTextUtf8 :: FsRoot root => Path root -> IO T.Text
readStrictTextUtf8 path = do
    bs <- readStrictByteString path
    evaluate (T.E.decodeUtf8 bs)

-- | Write lazy 'Text' to a file (using UTF-8 encoding). The file is truncated to zero length before writing begins.
writeLazyTextUtf8 :: FsRoot root => Path root -> T.L.Text -> IO ()
writeLazyTextUtf8 path bs = do
    filePath <- toAbsoluteFilePath path
    T.L.writeFile filePath bs

-- | Write strict 'Text' to a file (using UTF-8 encoding). The file is truncated to zero length before writing begins.
writeStrictTextUtf8 :: FsRoot root => Path root -> T.Text -> IO ()
writeStrictTextUtf8 path bs = do
    filePath <- toAbsoluteFilePath path
    T.writeFile filePath bs

-- | Append lazy 'Text' to end of a file (using UTF-8 encoding).
appendLazyTextUtf8 :: FsRoot root => Path root -> T.L.Text -> IO ()
appendLazyTextUtf8 path bs = do
    filePath <- toAbsoluteFilePath path
    T.L.appendFile filePath bs

-- | Append strict 'Text' to end of a file (using UTF-8 encoding).
appendStrictTextUtf8 :: FsRoot root => Path root -> T.Text -> IO ()
appendStrictTextUtf8 path bs = do
    filePath <- toAbsoluteFilePath path
    T.appendFile filePath bs
