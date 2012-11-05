{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- |
-- Boxed vector.
module Data.Vector.Fixed.Boxed (
    -- * Immutable
    Vec
  , Vec2
  , Vec3
    -- * Mutable
  , MVec
  ) where

import Control.Monad
import Data.Primitive.Array
import Prelude hiding (length,replicate,zipWith,map,foldl)

import Data.Vector.Fixed
import Data.Vector.Fixed.Internal
import Data.Vector.Fixed.Mutable



----------------------------------------------------------------
-- Data type
----------------------------------------------------------------

-- | Unboxed vector with fixed length
newtype Vec n a = Vec (Array a)

-- | Mutable unboxed vector with fixed length
newtype MVec n s a = MVec (MutableArray s a)

type Vec2 = Vec (S (S Z))
type Vec3 = Vec (S (S (S Z)))



----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

instance (Arity n, Show a) => Show (Vec n a) where
  show v = "fromList " ++ show (toList v)


type instance Mutable (Vec n) = MVec n

instance (Arity n) => MVector (MVec n) a where
  overlaps (MVec v) (MVec u) = sameMutableArray v u
  {-# INLINE overlaps    #-}
  new = do
    v <- newArray (arity (undefined :: n)) uninitialised
    return $ MVec v
  {-# INLINE new         #-}
  copy = move
  {-# INLINE copy        #-}
  move (MVec dst) (MVec src) = copyMutableArray dst 0 src 0 (arity (undefined :: n))
  {-# INLINE move        #-}
  unsafeRead  (MVec v) i   = readArray  v i
  {-# INLINE unsafeRead  #-}
  unsafeWrite (MVec v) i x = writeArray v i x
  {-# INLINE unsafeWrite #-}

instance (Arity n) => IVector (Vec n) a where
  unsafeFreeze (MVec v)   = do { a <- unsafeFreezeArray v; return $! Vec  a }
  unsafeThaw   (Vec  v)   = do { a <- unsafeThawArray   v; return $! MVec a }
  unsafeIndex  (Vec  v) i = indexArray v i
  {-# INLINE unsafeFreeze #-}
  {-# INLINE unsafeThaw   #-}
  {-# INLINE unsafeIndex  #-}



type instance Dim  (Vec  n) = n
type instance DimM (MVec n) = n

instance (Arity n) => Vector (Vec n) a where
  construct = constructVec
  inspect   = inspectVec
  {-# INLINE construct #-}
  {-# INLINE inspect   #-}

uninitialised :: a
uninitialised = error "Data.Vector.Fixed.Boxed: uninitialised element"
