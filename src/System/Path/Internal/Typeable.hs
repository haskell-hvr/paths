{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
module System.Path.Internal.Typeable (
    Typeable
  , typeRep
  ) where

#if MIN_VERSION_base(4,7,0)
import Data.Typeable (Typeable, typeRep)
#else
import Data.Typeable (Typeable, TypeRep, typeOf)

typeRep :: forall a proxy. Typeable a => proxy a -> TypeRep
typeRep _ = typeOf (undefined :: a)
#endif
