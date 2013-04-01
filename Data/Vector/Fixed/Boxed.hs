{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
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

import Control.Applicative  (Applicative(..))
import Data.Primitive.Array
import Data.Typeable        (Typeable)
import qualified Data.Foldable    as F
import qualified Data.Traversable as T
import Prelude hiding (length,replicate,zipWith,map,foldl,foldr)

import Data.Vector.Fixed
import Data.Vector.Fixed.Internal.Arity
import Data.Vector.Fixed.Mutable



----------------------------------------------------------------
-- Data type
----------------------------------------------------------------

-- | Vector with fixed length which can hold any value.
newtype Vec n a = Vec (Array a)
                  deriving (Typeable)

-- | Mutable unboxed vector with fixed length
newtype MVec n s a = MVec (MutableArray s a)
                     deriving (Typeable)

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
  construct  = constructVec
  inspect    = inspectVec
  basicIndex = index
  {-# INLINE construct  #-}
  {-# INLINE inspect    #-}
  {-# INLINE basicIndex #-}
instance (Arity n) => VectorN Vec n a

instance (Arity n, Eq a) => Eq (Vec n a) where
  (==) = eq
  {-# INLINE (==) #-}

instance Arity n => Functor (Vec n) where
  {-# INLINE fmap #-}
  fmap = map

instance Arity n => Applicative (Vec n) where
  pure  = replicate
  (<*>) = zipWith ($)
  {-# INLINE pure  #-}
  {-# INLINE (<*>) #-}

instance Arity n => F.Foldable (Vec n) where
  foldr = foldr
  {-# INLINE foldr #-}

instance Arity n => T.Traversable (Vec n) where
  sequenceA = sequenceA
  traverse  = traverse
  {-# INLINE sequenceA #-}
  {-# INLINE traverse #-}

uninitialised :: a
uninitialised = error "Data.Vector.Fixed.Boxed: uninitialised element"
