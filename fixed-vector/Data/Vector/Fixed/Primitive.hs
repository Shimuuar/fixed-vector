{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
-- |
-- Unboxed vectors with fixed length. Vectors from
-- "Data.Vector.Fixed.Unboxed" provide more flexibility at no
-- performeance cost.
module Data.Vector.Fixed.Primitive (
    -- * Immutable
    Vec
  , Vec2
  , Vec3
  , Vec4
  , Vec5
    -- * Mutable
  , MVec
    -- * Type classes
  , Prim
  ) where

import Control.Monad
import Data.Data
import Data.Monoid              (Monoid(..))
import Data.Primitive.ByteArray
import Data.Primitive
import Prelude hiding (length,replicate,zipWith,map,foldl)

import Data.Vector.Fixed hiding (index)
import Data.Vector.Fixed.Mutable
import qualified Data.Vector.Fixed.Cont as C



----------------------------------------------------------------
-- Data type
----------------------------------------------------------------

-- | Unboxed vector with fixed length
newtype Vec n a = Vec ByteArray

-- | Mutable unboxed vector with fixed length
newtype MVec n s a = MVec (MutableByteArray s)

deriving instance Typeable2 Vec
deriving instance Typeable3 MVec

type Vec2 = Vec (S (S Z))
type Vec3 = Vec (S (S (S Z)))
type Vec4 = Vec (S (S (S (S Z))))
type Vec5 = Vec (S (S (S (S (S Z)))))



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
  construct  = constructVec
  inspect    = inspectVec
  basicIndex = index
  {-# INLINE construct  #-}
  {-# INLINE inspect    #-}
  {-# INLINE basicIndex #-}
instance (Arity n, Prim a) => VectorN Vec n a

instance (Arity n, Prim a, Eq a) => Eq (Vec n a) where
  (==) = eq
  {-# INLINE (==) #-}
instance (Arity n, Prim a, Ord a) => Ord (Vec n a) where
  compare = ord
  {-# INLINE compare #-}

instance (Arity n, Prim a, Monoid a) => Monoid (Vec n a) where
  mempty  = replicate mempty
  mappend = zipWith mappend
  {-# INLINE mempty  #-}
  {-# INLINE mappend #-}

instance (Typeable n, Arity n, Prim a, Data a) => Data (Vec n a) where
  gfoldl       = C.gfoldl
  gunfold      = C.gunfold
  toConstr   _ = con_Vec
  dataTypeOf _ = ty_Vec

ty_Vec :: DataType
ty_Vec  = mkDataType "Data.Vector.Fixed.Primitive.Vec" [con_Vec]

con_Vec :: Constr
con_Vec = mkConstr ty_Vec "Vec" [] Prefix

