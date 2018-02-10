{-# LANGUAGE CPP  #-}
{-# LANGUAGE Safe #-}

-- compat layer
module System.Path.Internal.Compat
    ( Applicative(..)
    , (<$>)
    , dirMakeAbsolute
    ) where

#if !MIN_VERSION_base(4,8,0)
import           Control.Applicative
#endif
import qualified System.Directory    as Dir
#if !MIN_VERSION_directory(1,2,2)
import qualified System.FilePath     as FP.Native
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
