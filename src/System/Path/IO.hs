{-# LANGUAGE Safe #-}

-- | This module extends "System.Path" (which is re-exported for
-- convenience) with thin wrappers around common IO functions
-- and is intended to replace imports of "System.FilePath",
--
-- To facilitate importing this module unqualified we also re-export
-- some definitions from "System.IO" (importing both would likely lead
-- to name clashes).
module System.Path.IO
  (
    module System.Path
  , module System.Path.IO.ByteString
  , module System.Path.IO.Directory
  , module System.Path.IO.Handle
  , module System.Path.IO.Text
  ) where

import           System.Path
import           System.Path.IO.ByteString
import           System.Path.IO.Directory
import           System.Path.IO.Handle
import           System.Path.IO.Text
