{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ImpredicativeTypes #-}
-- |
-- Unboxed vectors with fixed length
module Data.Vector.Fixed.Unboxed (
    -- * Immutable
    Vec
  , Vec2
  , Vec3
    -- * Mutable
  , MVec
  ) where

import Control.Monad
import Data.Primitive.ByteArray
import Data.Primitive
import Prelude hiding (length,replicate,zipWith,map,foldl)

import Data.Vector.Fixed
import Data.Vector.Fixed.Mutable



----------------------------------------------------------------
-- Data type
----------------------------------------------------------------

-- | Unboxed vector with fixed length
newtype Vec n a = Vec ByteArray

-- | Mutable unboxed vector with fixed length
newtype MVec n s a = MVec (MutableByteArray s)

type Vec2 = Vec (S (S Z))
type Vec3 = Vec (S (S (S Z)))



----------------------------------------------------------------
-- Vector instance
----------------------------------------------------------------

type instance Mutable (Vec n) = MVec n

instance (Arity n, Prim a) => MVector (MVec n) a where
  lengthM _ = arity (undefined :: n)
  new = do
    v <- newByteArray $! arity (undefined :: n) * sizeOf (undefined :: a)
    return $ MVec v
  clone (MVec v) = do
    r@(MVec u) <- new
    copyMutableByteArray u 0 v 0 (arity (undefined :: n) * sizeOf (undefined :: a))
    return r
  unsafeRead  (MVec v) i   = readByteArray  v i
  unsafeWrite (MVec v) i x = writeByteArray v i x
  {-# INLINE lengthM     #-}
  {-# INLINE new         #-}
  {-# INLINE clone       #-}
  {-# INLINE unsafeRead  #-}
  {-# INLINE unsafeWrite #-}

instance (Arity n, Prim a) => IVector (Vec n) a where
  unsafeFreeze (MVec v)   = do { a <- unsafeFreezeByteArray v; return $! Vec  a }
  unsafeThaw   (Vec  v)   = do { a <- unsafeThawByteArray   v; return $! MVec a }
  unsafeIndex  (Vec  v) i = indexByteArray v i
  {-# INLINE unsafeFreeze #-}
  {-# INLINE unsafeThaw   #-}
  {-# INLINE unsafeIndex  #-}


type instance Dim (Vec n) = n

instance (Arity n, Prim a) => Vector (Vec n) a where
  construct = constructVec
  inspect   = inspectVec
  {-# INLINE construct #-}
  {-# INLINE inspect   #-}



----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

instance (Arity n, Prim a, Show a) => Show (Vec n a) where
  show = show . toList
