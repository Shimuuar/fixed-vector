{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
-- |
-- Type classes for vectors which are implemented on top of the arrays
-- and support in-place mutation. API is similar to one used in the
-- @vector@ package.
module Data.Vector.Fixed.Mutable (
    -- * Mutable vectors
    Arity
  , arity
  , Mutable
  , DimM
  , MVector(..)
  , lengthM
  , read
  , write
  , clone
    -- * Immutable vectors
  , IVector(..)
  , index
  , lengthI
  , freeze
  , thaw
    -- * Vector API
  , constructVec
  , inspectVec
  ) where

import Control.Applicative (Const(..))
import Control.Monad.ST
import Control.Monad.Primitive
import Data.Typeable  (Proxy(..))
import GHC.TypeLits
import Data.Vector.Fixed.Cont (Dim,PeanoNum(..),Peano,Arity,Fun(..),Vector(..),ContVec,arity,apply,accum,length)
import Prelude hiding (read,length)


----------------------------------------------------------------
-- Type classes
----------------------------------------------------------------

-- | Mutable counterpart of fixed-length vector.
type family Mutable (v :: * -> *) :: * -> * -> *

-- | Dimension for mutable vector.
type family DimM (v :: * -> * -> *) :: Nat

-- | Type class for mutable vectors.
class (Arity (DimM v)) => MVector v a where
  -- | Checks whether vectors' buffers overlaps
  overlaps  :: v s a -> v s a -> Bool
  -- | Copy vector. The two vectors may not overlap. Since vectors'
  --   length is encoded in the type there is no need in runtime checks.
  copy :: PrimMonad m
       => v (PrimState m) a    -- ^ Target
       -> v (PrimState m) a    -- ^ Source
       -> m ()
  -- | Copy vector. The two vectors may overlap. Since vectors' length
  --   is encoded in the type there is no need in runtime checks.
  move :: PrimMonad m
       => v (PrimState m) a    -- ^ Target
       -> v (PrimState m) a    -- ^ Source
       -> m ()
  -- | Allocate new vector
  new   :: PrimMonad m => m (v (PrimState m) a)
  -- | Read value at index without bound checks.
  unsafeRead  :: PrimMonad m => v (PrimState m) a -> Int -> m a
  -- | Write value at index without bound checks.
  unsafeWrite :: PrimMonad m => v (PrimState m) a -> Int -> a -> m ()


-- | Length of mutable vector. Function doesn't evaluate its argument.
lengthM :: forall v s a. (Arity (DimM v)) => v s a -> Int
lengthM _ = arity (Proxy :: Proxy (DimM v))

-- | Create copy of vector.
clone :: (PrimMonad m, MVector v a) => v (PrimState m) a -> m (v (PrimState m) a)
{-# INLINE clone #-}
clone v = do
  u <- new
  move v u
  return u

-- | Read value at index with bound checks.
read  :: (PrimMonad m, MVector v a) => v (PrimState m) a -> Int -> m a
{-# INLINE read #-}
read v i
  | i < 0 || i >= lengthM v = error "Data.Vector.Fixed.Mutable.read: index out of range"
  | otherwise               = unsafeRead v i

-- | Write value at index with bound checks.
write :: (PrimMonad m, MVector v a) => v (PrimState m) a -> Int -> a -> m ()
{-# INLINE write #-}
write v i x
  | i < 0 || i >= lengthM v = error "Data.Vector.Fixed.Mutable.write: index out of range"
  | otherwise               = unsafeWrite v i x


-- | Type class for immutable vectors
class (Dim v ~ DimM (Mutable v), MVector (Mutable v) a) => IVector v a where
  -- | Convert vector to immutable state. Mutable vector must not be
  --   modified afterwards.
  unsafeFreeze :: PrimMonad m => Mutable v (PrimState m) a -> m (v a)
  -- | Convert immutable vector to mutable. Immutable vector must not
  --   be used afterwards.
  unsafeThaw   :: PrimMonad m => v a -> m (Mutable v (PrimState m) a)
  -- | Get element at specified index without bounds check.
  unsafeIndex :: v a -> Int -> a

-- | Length of immutable vector. Function doesn't evaluate its argument.
lengthI :: IVector v a => v a -> Int
lengthI = length

index :: IVector v a => v a -> Int -> a
{-# INLINE index #-}
index v i | i < 0 || i >= lengthI v = error "Data.Vector.Fixed.Mutable.!: index out of bounds"
          | otherwise               = unsafeIndex v i


-- | Safely convert mutable vector to immutable.
freeze :: (PrimMonad m, IVector v a) => Mutable v (PrimState m) a -> m (v a)
{-# INLINE freeze #-}
freeze v = unsafeFreeze =<< clone v

-- | Safely convert immutable vector to mutable.
thaw :: (PrimMonad m, IVector v a) => v a -> m (Mutable v (PrimState m) a)
{-# INLINE thaw #-}
thaw v = clone =<< unsafeThaw v



----------------------------------------------------------------
-- Vector API
----------------------------------------------------------------

-- | Generic inspect implementation for array-based vectors.
inspectVec :: forall v a b. (Arity (Dim v), IVector v a) => v a -> Fun (Peano (Dim v)) a b -> b
{-# INLINE inspectVec #-}
inspectVec v
  = inspect cv
  where
    cv :: ContVec (Dim v) a
    cv = apply (\(Const i) -> (unsafeIndex v i, Const (i+1)))
               (Const 0 :: Const Int (Peano (Dim v)))


-- | Generic construct implementation for array-based vectors.
constructVec :: forall v a. (Arity (Dim v), IVector v a) => Fun (Peano (Dim v)) a (v a)
{-# INLINE constructVec #-}
constructVec =
  accum step
        (\(T_new _ st) -> runST $ unsafeFreeze =<< st :: v a)
        (T_new 0 new :: T_new v a (Peano (Dim v)))

data T_new v a n = T_new Int (forall s. ST s (Mutable v s a))

step :: (IVector v a) => T_new v a ('S n) -> a -> T_new v a n
step (T_new i st) x = T_new (i+1) $ do
  mv <- st
  unsafeWrite mv i x
  return mv
