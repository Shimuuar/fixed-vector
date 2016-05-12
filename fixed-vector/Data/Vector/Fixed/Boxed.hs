{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
-- |
-- Vector which could hold any value.
module Data.Vector.Fixed.Boxed (
    -- * Immutable
    Vec
  , Vec1
  , Vec2
  , Vec3
  , Vec4
  , Vec5
    -- * Mutable
  , MVec
  ) where

import Control.Applicative  (Applicative(..))
import Control.DeepSeq      (NFData(..))
import Data.Primitive.Array
import Data.Monoid          (Monoid(..))
import Data.Data
import qualified Data.Foldable    as F
import qualified Data.Traversable as T
import Foreign.Storable (Storable(..))
import Prelude (Show(..),Eq(..),Ord(..),Functor(..),Monad(..))
import Prelude ((++),($),($!),undefined,error,seq)

import Data.Vector.Fixed hiding (index)
import Data.Vector.Fixed.Mutable
import qualified Data.Vector.Fixed.Cont as C



----------------------------------------------------------------
-- Data type
----------------------------------------------------------------

-- | Vector with fixed length which can hold any value.
newtype Vec n a = Vec (Array a)

-- | Mutable unboxed vector with fixed length
newtype MVec n s a = MVec (MutableArray s a)

deriving instance Typeable Vec
deriving instance Typeable MVec

type Vec1 = Vec (S Z)
type Vec2 = Vec (S (S Z))
type Vec3 = Vec (S (S (S Z)))
type Vec4 = Vec (S (S (S (S Z))))
type Vec5 = Vec (S (S (S (S (S Z)))))


instance (Typeable n, Arity n, Data a) => Data (Vec n a) where
  gfoldl       = C.gfoldl
  gunfold      = C.gunfold
  toConstr   _ = con_Vec
  dataTypeOf _ = ty_Vec

ty_Vec :: DataType
ty_Vec  = mkDataType "Data.Vector.Fixed.Boxed.Vec" [con_Vec]

con_Vec :: Constr
con_Vec = mkConstr ty_Vec "Vec" [] Prefix

instance (Storable a, Arity n) => Storable (Vec n a) where
  alignment = defaultAlignemnt
  sizeOf    = defaultSizeOf
  peek      = defaultPeek
  poke      = defaultPoke
  {-# INLINE alignment #-}
  {-# INLINE sizeOf    #-}
  {-# INLINE peek      #-}
  {-# INLINE poke      #-}




----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

instance (Arity n, Show a) => Show (Vec n a) where
  show v = "fromList " ++ show (toList v)

instance (Arity n, NFData a) => NFData (Vec n a) where
  rnf = foldl (\r a -> r `seq` rnf a) ()
  {-# INLINE rnf #-}

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
instance (Arity n, Ord a) => Ord (Vec n a) where
  compare = ord
  {-# INLINE compare #-}

instance (Arity n, Monoid a) => Monoid (Vec n a) where
  mempty  = replicate mempty
  mappend = zipWith mappend
  {-# INLINE mempty  #-}
  {-# INLINE mappend #-}

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
