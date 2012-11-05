{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- |
-- Unboxed vectors with fixed length. Vectors from
-- "Data.Vector.Fixed.Unboxed" provide more flexibility at no
-- performeance cost.
module Data.Vector.Fixed.Primitive (
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
import Data.Vector.Fixed.Internal
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

{-# RULES "fixed-vector: !/index"
    (!) = index :: Arity n => Vec n a -> Int -> a
  #-}



----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

instance (Arity n, Prim a, Show a) => Show (Vec n a) where
  show v = "fromList " ++ show (toList v)



type instance Mutable (Vec n) = MVec n

instance (Arity n, Prim a) => MVector (MVec n) a where
  overlaps (MVec v) (MVec u) = sameMutableByteArray v u
  {-# INLINE overlaps    #-}
  new = do
    v <- newByteArray $! arity (undefined :: n) * sizeOf (undefined :: a)
    return $ MVec v
  {-# INLINE new         #-}
  copy                       = move
  {-# INLINE copy        #-}
  move (MVec dst) (MVec src) = copyMutableByteArray dst 0 src 0 (arity (undefined :: n))
  {-# INLINE move        #-}
  unsafeRead  (MVec v) i   = readByteArray  v i
  {-# INLINE unsafeRead  #-}
  unsafeWrite (MVec v) i x = writeByteArray v i x
  {-# INLINE unsafeWrite #-}

instance (Arity n, Prim a) => IVector (Vec n) a where
  unsafeFreeze (MVec v)   = do { a <- unsafeFreezeByteArray v; return $! Vec  a }
  unsafeThaw   (Vec  v)   = do { a <- unsafeThawByteArray   v; return $! MVec a }
  unsafeIndex  (Vec  v) i = indexByteArray v i
  {-# INLINE unsafeFreeze #-}
  {-# INLINE unsafeThaw   #-}
  {-# INLINE unsafeIndex  #-}



type instance Dim  (Vec  n) = n
type instance DimM (MVec n) = n

instance (Arity n, Prim a) => Vector (Vec n) a where
  construct = constructVec
  inspect   = inspectVec
  {-# INLINE construct #-}
  {-# INLINE inspect   #-}
