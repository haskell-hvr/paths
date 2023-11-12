{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Trustworthy               #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module System.Path.Internal (
    -- * Paths
    Path(..)
  , FileExt(..)
    -- * FilePath-like operations on paths with arbitrary roots
  , takeDirectory
  , takeFileName
  , takeBaseName
  , normalise

  , (<.>)
  , (-<.>)
  , splitExtension
  , splitExtensions
  , takeExtension
  , takeExtensions
  , stripExtension
  , isExtensionOf

    -- ** Trailing slash functions
  , hasTrailingPathSeparator
  , addTrailingPathSeparator
  , dropTrailingPathSeparator
    -- * Unrooted paths
  , Unrooted
  , (</>)
  , unrootPath
  , toUnrootedFilePath
  , fromUnrootedFilePath
  , fragment
  , fragments
  , joinFragments
  , splitFragments
--  , isPathPrefixOf
    -- * File-system paths
  , FsRoot(toAbsoluteFilePath)
  , FsUniqueRoot(..)
  , FsPath(..)
  , CWD
  , Relative
  , Absolute
  , HomeDir
    -- ** XDG roots
  , XdgData
  , XdgConfig
  , XdgCache
    -- ** Conversions
  , toFilePath
  , fromFilePath
  , MakeAbsolute (..)
  , fromAbsoluteFilePath
{-
    -- * Wrappers around Codec.Archive.Tar
  , Tar
  , tarIndexLookup
  , tarAppend
    -- * Wrappers around Network.URI
  , Web
  , toURIPath
  , fromURIPath
  , uriPath
  , modifyUriPath
-}
  ) where

import           Control.DeepSeq             (NFData (rnf))
import           Data.List                   (stripPrefix)
import qualified System.Directory            as Dir
import qualified System.FilePath             as FP.Native
import qualified System.FilePath.Posix       as FP.Posix

import           System.Path.Internal.Compat as Compat
import           System.Path.Internal.Native
import           System.Path.Internal.Typeable

{-------------------------------------------------------------------------------
  Paths
-------------------------------------------------------------------------------}

-- | Paths
--
-- A 'Path' is a wrapped 'FilePath' with a type-level tag indicating where this
-- path is rooted (relative to the current directory, absolute path, relative to
-- a web domain, whatever). Most operations on 'Path' are just lifted versions
-- of the operations on the underlying 'FilePath'. The tag however allows us to
-- give a lot of operations a more meaningful type. For instance, it does not
-- make sense to append two absolute paths together; instead, we can only append
-- an unrooted path to another path. It also means we avoid bugs where we use
-- one kind of path where we expect another.
newtype Path a = Path FilePath -- always a Posix style path internally
               deriving (Show, Eq, Ord, Typeable)

instance NFData (Path a) where
    rnf (Path p) = rnf p

mkPathNative :: FilePath -> Path a
mkPathNative = Path . posixFromNative

unPathNative :: Path a -> FilePath
unPathNative (Path fp) = posixToNative fp

mkPathPosix :: FilePath -> Path a
mkPathPosix = Path

unPathPosix :: Path a -> FilePath
unPathPosix (Path fp) = fp

{-------------------------------------------------------------------------------
  FilePath-like operations on paths with an arbitrary root
-------------------------------------------------------------------------------}

----------------------------------------------------------------------------
-- file extensions

-- | Type to represent filepath extensions.
--
-- File extensions are usually a high-level convention and in most
-- cases the low-level filesystem layer is agnostic to them.
--
-- @since 0.2.0.0
newtype FileExt = FileExt String
                deriving (Show, Eq, Ord, Typeable)

infixr 7  <.>, -<.>

-- | Wrapped 'FP.Posix.<.>'
(<.>) :: Path a -> FileExt -> Path a
fp <.> (FileExt ext) = liftFP (FP.Posix.<.> ('.':ext)) fp

-- | Wrapped 'FP.Posix.-<.>'
--
-- @since 0.2.0.0
(-<.>) :: Path a -> FileExt -> Path a
fp -<.> (FileExt ext) = liftFP (flip FP.Posix.replaceExtension ('.':ext)) fp

-- | Wrapped 'FP.Posix.splitExtension'
splitExtension :: Path a -> (Path a, Maybe FileExt)
splitExtension (Path fp)
  = case FP.Posix.splitExtension fp of
      (fp', "") -> (Path fp', Nothing)
      (fp', '.':ext) -> (Path fp', Just (FileExt ext))
      _ -> error "System.Path.splitExtension: the impossible happened"

-- | Wrapped 'FP.Posix.takeExtension'
takeExtension :: Path a -> Maybe FileExt
takeExtension (Path fp)
  = case FP.Posix.takeExtension fp of
      ""      -> Nothing
      '.':ext -> Just (FileExt ext)
      _       -> error "System.Path.takeExtension: the impossible happened"


-- | Wrapped 'FP.Posix.splitExtensions'
--
-- @since 0.2.0.0
splitExtensions :: Path a -> (Path a, Maybe FileExt)
splitExtensions (Path fp)
  = case FP.Posix.splitExtensions fp of
      (fp', "") -> (Path fp', Nothing)
      (fp', '.':ext) -> (Path fp', Just (FileExt ext))
      _ -> error "System.Path.splitExtension: the impossible happened"

-- | Wrapped 'FP.Posix.takeExtensions'
--
-- @since 0.2.0.0
takeExtensions :: Path a -> Maybe FileExt
takeExtensions (Path fp)
  = case FP.Posix.takeExtension fp of
      ""      -> Nothing
      '.':ext -> Just (FileExt ext)
      _       -> error "System.Path.takeExtension: the impossible happened"


-- | Wrapped 'FP.Posix.stripExtension'
--
-- @since 0.2.0.0
stripExtension :: FileExt -> Path a -> Maybe (Path a)
stripExtension (FileExt ext) (Path fp) = fmap Path (stripExtension' ext fp)
  where
    stripExtension' []         path = Just path
    stripExtension' ext'@(x:_) path = stripSuffix (if FP.Posix.isExtSeparator x then ext' else '.':ext') path
      where
        stripSuffix xs ys = reverse <$> stripPrefix (reverse xs) (reverse ys)

-- | Wrapped 'FP.Posix.isExtensionOf'
--
-- @since 0.2.0.0
isExtensionOf :: FileExt -> Path a -> Bool
isExtensionOf (FileExt ext) (Path fp) = posixIsExtensionOf ext fp

----------------------------------------------------------------------------

-- | Wrapped 'FP.Posix.takeDirectory'
--
takeDirectory :: Path a -> Path a
takeDirectory = liftFP FP.Posix.takeDirectory

-- | Normalise 'Path' according to POSIX rules.
--
-- See documentation of 'FP.Posix.normalise' for details.
--
-- @since 0.2.0.0
normalise :: Path a -> Path a
normalise = liftFP FP.Posix.normalise

{-------------------------------------------------------------------------------
  Unrooted paths
-------------------------------------------------------------------------------}

-- | Type-level tag for unrooted paths
--
-- Unrooted paths need a root before they can be interpreted.
data Unrooted deriving Typeable

-- instance Pretty (Path Unrooted) where
--   pretty (Path fp) = fp

-- | Wrapped 'FP.Posix.takeFileName'
takeFileName :: Path a -> Path Unrooted
takeFileName = liftFP FP.Posix.takeFileName

-- | Wrapped 'FP.Posix.takeBaseName'
--
-- @since 0.2.0.0
takeBaseName :: Path a -> Path Unrooted
takeBaseName = fst . splitExtension . takeFileName

-- NB: we don't wrap splitFileName for now, as confusingly,
-- 'takeDirectory' is not the same as 'fst . splitFileName'

infixr 5 </>

-- | Wrapped 'FP.Posix.</>'
--
-- The empty fragment @fragment ""@ acts as the right-identity
--
-- @root </> 'fragment' "" == root@
--
-- This is the inverse to 'splitFileName'.
(</>) :: Path a -> Path Unrooted -> Path a
(</>) = liftFP2 (FP.Posix.</>)

-- | Wrapped 'FP.Posix.hasTrailingPathSeparator'
--
-- @since 0.2.0.0
hasTrailingPathSeparator :: Path a -> Bool
hasTrailingPathSeparator (Path fp) = FP.Posix.hasTrailingPathSeparator fp

-- | Wrapped 'FP.Posix.addTrailingPathSeparator'
--
-- @since 0.2.0.0
addTrailingPathSeparator :: Path a -> Path a
addTrailingPathSeparator = liftFP FP.Posix.addTrailingPathSeparator

-- | Wrapped 'FP.Posix.dropTrailingPathSeparator'
--
-- @since 0.2.0.0
dropTrailingPathSeparator :: Path a -> Path a
dropTrailingPathSeparator = liftFP FP.Posix.dropTrailingPathSeparator

-- | Forget a path's root
--
-- __NOTE__: If the original 'Path' is considered an absolute POSIX style
-- FilePath, it's automatically converted to a relative FilePath.
unrootPath :: Path root -> Path Unrooted
unrootPath = liftFP FP.Posix.dropDrive

-- | Convert a relative\/unrooted Path to a FilePath (using POSIX style
-- directory separators).
--
-- See also 'toAbsoluteFilePath'
--
toUnrootedFilePath :: Path Unrooted -> FilePath
toUnrootedFilePath = unPathPosix

-- | Convert from a relative\/unrooted FilePath (using POSIX style directory
-- separators).
--
-- __NOTE__: If the argument is considered an absolute POSIX style
-- FilePath, it's automatically converted to a relative FilePath.
fromUnrootedFilePath :: FilePath -> Path Unrooted
fromUnrootedFilePath = mkPathPosix . FP.Posix.dropDrive

-- | A path fragment (like a single directory or filename)
--
-- __NOTE__: If the argument would be considered an absolute POSIX style
-- FilePath, it's automatically converted to a relative FilePath.
fragment :: String -> Path Unrooted
fragment = Path . FP.Posix.dropDrive

-- | Version of 'fragment' taking a list of fragments
--
-- __NOTE__: If any argument would be considered an absolute POSIX style
-- FilePath, it's automatically converted to a relative FilePath.
--
-- @since 0.2.0.0
fragments :: [String] -> Path Unrooted
fragments = liftToFP (FP.Posix.joinPath . map FP.Posix.dropDrive)

-- | Wrapped 'FP.Posix.joinPath'
--
-- @since 0.2.0.0
joinFragments :: [Path Unrooted] -> Path Unrooted
joinFragments fs = Path (FP.Posix.joinPath [ f | Path f <- fs ])

-- | Wrapped 'FP.Posix.splitDirectories'
--
-- @since 0.2.0.0
splitFragments :: Path Unrooted -> [Path Unrooted]
splitFragments (Path fp) = map Path (FP.Posix.splitDirectories fp)

-- FIXME
-- isPathPrefixOf :: Path Unrooted -> Path Unrooted -> Bool
-- isPathPrefixOf = liftFromFP2 isPrefixOf

{-------------------------------------------------------------------------------
  File-system paths
-------------------------------------------------------------------------------}

-- | Compatibility type-synonym
type Relative = CWD
{-# DEPRECATED Relative "Please use 'CWD' instead" #-}

-- | 'Path' tag for paths /rooted/ at the /current working directory/
--
-- @since 0.2.0.0
data CWD deriving Typeable

-- | 'Path' tag for absolute paths
data Absolute deriving Typeable

-- | 'Path' tag for paths /rooted/ at @$HOME@
data HomeDir deriving Typeable

-- instance Pretty (Path Absolute) where
--   pretty (Path fp) = fp

-- instance Pretty (Path CWD) where
--   pretty (Path fp) = "./" ++ fp

-- instance Pretty (Path HomeDir) where
--   pretty (Path fp) = "~/" ++ fp

-- | A file system root can be interpreted as an (absolute) FilePath
class FsRoot root where
  -- | Convert a Path to an absolute native FilePath (using native style directory separators).
  --
  -- This operation needs to be in 'IO' for resolving paths with
  -- dynamic roots, such as 'CWD' or 'HomeDir'.
  --
  -- See also 'makeAbsolute'
  toAbsoluteFilePath :: Path root -> IO FilePath

  -- this member is hidden, but should be derivable for any type
  rootName :: Proxy root -> String
  default rootName :: Typeable root => Proxy root -> String
  rootName = show . typeRep

-- | A file system root with a unique root.
--
-- For example 'Absolute' isn't 'FsUniqueRoot' on Windows,
-- as there are multiple drives.
--
class FsRoot root => FsUniqueRoot root where
  -- | Path representing the base path
  basePath :: Path root

-- proxy for rootName
data Proxy a = Proxy

instance FsRoot CWD where
    toAbsoluteFilePath p = dirMakeAbsolute (unPathNative p)

instance FsRoot Absolute where
    toAbsoluteFilePath = return . unPathNative

instance FsRoot HomeDir where
    toAbsoluteFilePath p = do
      home <- Dir.getHomeDirectory
      return $ home FP.Native.</> unPathNative p

-- |
--
-- >>> basePath </> fromUnrootedFilePath "here" </> fromUnrootedFilePath "there" :: Path CWD
-- Path "here/there"
instance FsUniqueRoot CWD where
    basePath = Path ""

-- |
--
-- >>> basePath </> fromUnrootedFilePath "here" </> fromUnrootedFilePath "there" :: Path HomeDir
-- Path "here/there"
instance FsUniqueRoot HomeDir where
    basePath = Path ""

-- | Abstract over a file system root
--
-- 'FsPath' can be constructed directly or via 'fromFilePath' or 'System.Path.QQ.fspath'.
--
-- >>> fromFilePath "/dev/null"
-- FsRoot @Absolute (Path "/dev/null")
--
-- >>> fromFilePath "foo/bar"
-- FsRoot @CWD (Path "foo/bar")
--
-- >>> fromFilePath "~/.foo"
-- FsRoot @HomeDir (Path ".foo")
--
data FsPath = forall root. FsRoot root => FsPath (Path root)
            deriving Typeable

instance Show FsPath where
    showsPrec d (FsPath path) = f path where
        f :: forall root. FsRoot root => Path root -> ShowS
        f p = showParen (d > 10)
            $ showString "FsRoot @"
            . showString (rootName (Proxy :: Proxy root))
            . showChar ' '
            . showsPrec 11 p

instance NFData FsPath where
    rnf (FsPath a) = rnf a

{-------------------------------------------------------------------------------
  XDG roots
-------------------------------------------------------------------------------}

-- | For data files (e.g. images).
data XdgData deriving Typeable

-- | For configuration files.
data XdgConfig deriving Typeable

-- | For non-essential files (e.g. cache).
data XdgCache deriving Typeable

instance FsRoot XdgData   where toAbsoluteFilePath p = Compat.getXdgDirectory Compat.XdgData   (unPathNative p)
instance FsRoot XdgConfig where toAbsoluteFilePath p = Compat.getXdgDirectory Compat.XdgConfig (unPathNative p)
instance FsRoot XdgCache  where toAbsoluteFilePath p = Compat.getXdgDirectory Compat.XdgCache  (unPathNative p)

instance FsUniqueRoot XdgData   where basePath = Path ""
instance FsUniqueRoot XdgConfig where basePath = Path ""
instance FsUniqueRoot XdgCache  where basePath = Path ""

{-------------------------------------------------------------------------------
  Conversions
-------------------------------------------------------------------------------}

-- | Construct a 'FsPath' from a native 'FilePath'.
--
-- __NOTE__: Native 'FilePath's whose first path component is a @~@
-- (and not preceded by anything else) are interpreted to be relative
-- to @$HOME@ (even on non-POSIX systems).
fromFilePath :: FilePath -> FsPath
fromFilePath fp
    | FP.Native.isAbsolute fp = FsPath (mkPathNative fp  :: Path Absolute)
    | Just fp' <- atHome fp   = FsPath (mkPathNative fp' :: Path HomeDir)
    | otherwise               = FsPath (mkPathNative fp  :: Path CWD)
  where
    -- TODO: I don't know if there a standard way that Windows users refer to
    -- their home directory. For now, we'll only interpret '~'. Everybody else
    -- can specify an absolute path if this doesn't work.
    atHome :: FilePath -> Maybe FilePath
    atHome "~"           = Just ""
    atHome ('~':sep:fp') | FP.Native.isPathSeparator sep = Just fp'
    atHome _otherwise    = Nothing

class MakeAbsolute path where
    -- | Export some path to an absolute 'Path'
    --
    -- See also 'toAbsoluteFilePath'
    makeAbsolute :: path -> IO (Path Absolute)

instance MakeAbsolute FsPath where
    makeAbsolute (FsPath p) = mkPathNative <$> toAbsoluteFilePath p

instance FsRoot root => MakeAbsolute (Path root) where
    makeAbsolute p = makeAbsolute (FsPath p)

-- | Export absolute path to a native 'FilePath'.
--
-- This is the inverse to 'fromAbsoluteFilePath'.
--
toFilePath :: Path Absolute -> FilePath
toFilePath = unPathNative

-- | Construct 'Absolute' path from a native 'FilePath'.
--
-- This is the inverse to 'toFilePath'.
--
-- __NOTE__: If the argument is not an absolute path this function will throw an 'error'.
fromAbsoluteFilePath :: FilePath -> Path Absolute
fromAbsoluteFilePath fp
  | FP.Native.isAbsolute fp = mkPathNative fp
  | otherwise               = error "fromAbsoluteFilePath: not an absolute path"

{-------------------------------------------------------------------------------
  Wrappers around Codec.Archive.Tar.*
-------------------------------------------------------------------------------

data Tar

instance Pretty (Path Tar) where
  pretty (Path fp) = "<tarball>/" ++ fp

tarIndexLookup :: TarIndex.TarIndex -> Path Tar -> Maybe TarIndex.TarIndexEntry
tarIndexLookup index path = TarIndex.lookup index path'
  where
    path' :: FilePath
    path' = toUnrootedFilePath $ unrootPath path

tarAppend :: (FsRoot root, FsRoot root')
          => Path root   -- ^ Path of the @.tar@ file
          -> Path root'  -- ^ Base directory
          -> [Path Tar]  -- ^ Files to add, relative to the base dir
          -> IO ()
tarAppend tarFile baseDir contents = do
    tarFile' <- toAbsoluteFilePath tarFile
    baseDir' <- toAbsoluteFilePath baseDir
    Tar.append tarFile' baseDir' contents'
  where
    contents' :: [FilePath]
    contents' = map (unPathNative . unrootPath) contents

-------------------------------------------------------------------------------
  Wrappers around Network.URI
-------------------------------------------------------------------------------

data Web

toURIPath :: FilePath -> Path Web
toURIPath = rootPath . fromUnrootedFilePath

fromURIPath :: Path Web -> FilePath
fromURIPath = toUnrootedFilePath . unrootPath

uriPath :: URI.URI -> Path Web
uriPath = toURIPath . URI.uriPath

modifyUriPath :: URI.URI -> (Path Web -> Path Web) -> URI.URI
modifyUriPath uri f = uri { URI.uriPath = f' (URI.uriPath uri) }
  where
    f' :: FilePath -> FilePath
    f' = fromURIPath . f . toURIPath

-}

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

liftFP :: (FilePath -> FilePath) -> Path a -> Path b
liftFP f (Path fp) = Path (f fp)

liftFP2 :: (FilePath -> FilePath -> FilePath) -> Path a -> Path b -> Path c
liftFP2 f (Path fp) (Path fp') = Path (f fp fp')

-- liftFromFP :: (FilePath -> x) -> Path a -> x
-- liftFromFP f (Path fp) = f fp

-- liftFromFP2 :: (FilePath -> FilePath -> x) -> Path a -> Path b -> x
-- liftFromFP2 f (Path fp) (Path fp') = f fp fp'

liftToFP :: (x -> FilePath) -> x -> Path a
liftToFP f x = Path (f x)
