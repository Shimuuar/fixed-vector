{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies #-}
-- |
-- Type classes for mutable vectors. They are quite similar to ones
-- from @vector@ package but those only suitable for vectors with
-- variable length.
module Data.Vector.Fixed.Mutable (
    -- * Mutable vectors
    Mutable
  , MVector(..)
  , read
  , write
    -- * Immutable vectors
  , IVector(..)
  , lengthI
  , freeze
  , thaw
  ) where

import Control.Monad.Primitive
import Prelude hiding (read)


----------------------------------------------------------------
-- Type classes
----------------------------------------------------------------

-- | Mutable counterpart of fixed-length vector
type family Mutable (v :: * -> *) :: * -> * -> *

-- | Type class for mutable vectors
class MVector v a where
  -- | Number of elements. Function should be lazy in its argument.
  lengthM     :: v s a -> Int
  -- | Allocate new vector
  new   :: PrimMonad m => m (v (PrimState m) a)
  -- | Clone vector
  clone :: PrimMonad m => v (PrimState m) a -> m (v (PrimState m) a)
  -- | Read value at index without bound checks.
  unsafeRead  :: PrimMonad m => v (PrimState m) a -> Int -> m a
  -- | Wrtie value at index without bound checks.
  unsafeWrite :: PrimMonad m => v (PrimState m) a -> Int -> a -> m ()

read  :: (PrimMonad m, MVector v a) => v (PrimState m) a -> Int -> m a
{-# INLINE read #-}
read v i
  | i < 0 || i >= lengthM v = error "Data.Vector.Fixed.Mutable.read: index out of range"
  | otherwise               = unsafeRead v i

write :: (PrimMonad m, MVector v a) => v (PrimState m) a -> Int -> a -> m ()
write v i x
  | i < 0 || i >= lengthM v = error "Data.Vector.Fixed.Mutable.write: index out of range"
  | otherwise               = unsafeWrite v i x


-- | Type class for immutable vectors
class (MVector (Mutable v) a) => IVector v a where
  -- | Convert vector to immutable state. Mutable vector must not be
  --   modified afterwards.
  unsafeFreeze :: PrimMonad m => Mutable v (PrimState m) a -> m (v a)
  -- | Convert immutable vector to mutable. Immutable vector must not
  --   be used afterwards.
  unsafeThaw   :: PrimMonad m => v a -> m (Mutable v (PrimState m) a)

-- | Length of immutable vector
lengthI :: IVector v a => v a -> Int
lengthI = lengthM . cast
  where
    cast :: v a -> Mutable v () a
    cast _ = undefined

-- | Safely convert mutable vector to immutable.
freeze :: (PrimMonad m, IVector v a) => Mutable v (PrimState m) a -> m (v a)
{-# INLINE freeze #-}
freeze v = unsafeFreeze =<< clone v

-- | Safely convert immutable vector to mutable.
thaw :: (PrimMonad m, IVector v a) => v a -> m (Mutable v (PrimState m) a)
{-# INLINE thaw #-}
thaw v = clone =<< unsafeThaw v
