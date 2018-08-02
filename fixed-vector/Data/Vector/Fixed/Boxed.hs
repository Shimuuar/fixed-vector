{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
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
import Data.Primitive.SmallArray
import Data.Monoid          (Monoid(..))
import Data.Semigroup       (Semigroup(..))
import Data.Data
import qualified Data.Foldable    as F
import qualified Data.Traversable as T
import Foreign.Storable (Storable(..))
import GHC.TypeLits
import Prelude ( Show(..),Eq(..),Ord(..),Functor(..),Monad(..)
               , (++),($),($!),error,seq)

import Data.Vector.Fixed hiding (index)
import Data.Vector.Fixed.Mutable
import qualified Data.Vector.Fixed.Cont     as C
import qualified Data.Vector.Fixed.Internal as I



----------------------------------------------------------------
-- Data type
----------------------------------------------------------------

-- | Vector with fixed length which can hold any value.
newtype Vec (n :: Nat) a = Vec (SmallArray a)

-- | Mutable unboxed vector with fixed length
newtype MVec (n :: Nat) s a = MVec (SmallMutableArray s a)

deriving instance Typeable Vec
deriving instance Typeable MVec

type Vec1 = Vec 1
type Vec2 = Vec 2
type Vec3 = Vec 3
type Vec4 = Vec 4
type Vec5 = Vec 5


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
  showsPrec = I.showsPrec

instance (Arity n, NFData a) => NFData (Vec n a) where
  rnf = foldl (\r a -> r `seq` rnf a) ()
  {-# INLINE rnf #-}

type instance Mutable (Vec n) = MVec n

instance (Arity n) => MVector (MVec n) a where
  new = do
    v <- newSmallArray (arity (Proxy :: Proxy n)) uninitialised
    return $ MVec v
  {-# INLINE new         #-}
  copy = move
  {-# INLINE copy        #-}
  move (MVec dst) (MVec src) = copySmallMutableArray dst 0 src 0 (arity (Proxy :: Proxy n))
  {-# INLINE move        #-}
  unsafeRead  (MVec v) i   = readSmallArray  v i
  {-# INLINE unsafeRead  #-}
  unsafeWrite (MVec v) i x = writeSmallArray v i x
  {-# INLINE unsafeWrite #-}

instance (Arity n) => IVector (Vec n) a where
  unsafeFreeze (MVec v)   = do { a <- unsafeFreezeSmallArray v; return $! Vec  a }
  unsafeThaw   (Vec  v)   = do { a <- unsafeThawSmallArray   v; return $! MVec a }
  unsafeIndex  (Vec  v) i = indexSmallArray v i
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

instance (Arity n, Semigroup a) => Semigroup (Vec n a) where
  (<>) = zipWith (<>)
  {-# INLINE (<>) #-}

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
