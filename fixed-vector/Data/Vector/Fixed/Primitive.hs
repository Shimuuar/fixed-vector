{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Unboxed vectors with fixed length. Vectors from
-- "Data.Vector.Fixed.Unboxed" provide more flexibility at no
-- performeance cost.
module Data.Vector.Fixed.Primitive (
    -- * Immutable
    Vec
  , Vec1
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
import Control.DeepSeq (NFData(..))
import Data.Data
import Data.Monoid              (Monoid(..))
import Data.Semigroup           (Semigroup(..))
import Data.Primitive.ByteArray
import Data.Primitive
import qualified Foreign.Storable as Foreign (Storable(..))
import GHC.TypeLits
import GHC.Exts (proxy#)
import Prelude (Show(..),Eq(..),Ord(..),Num(..))
import Prelude (($),($!),undefined,seq)


import Data.Vector.Fixed hiding (index)
import Data.Vector.Fixed.Mutable (Mutable, MVector(..), IVector(..), DimM, constructVec, inspectVec, index)
import qualified Data.Vector.Fixed.Cont     as C
import           Data.Vector.Fixed.Cont     (Peano,Arity(..))
import qualified Data.Vector.Fixed.Internal as I



----------------------------------------------------------------
-- Data type
----------------------------------------------------------------

-- | Unboxed vector with fixed length
newtype Vec (n :: Nat) a = Vec ByteArray

-- | Mutable unboxed vector with fixed length
newtype MVec (n :: Nat) s a = MVec (MutableByteArray s)

deriving instance Typeable Vec
deriving instance Typeable MVec

type Vec1 = Vec 1
type Vec2 = Vec 2
type Vec3 = Vec 3
type Vec4 = Vec 4
type Vec5 = Vec 5



----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

instance (Arity (Peano n), Prim a, Show a) => Show (Vec n a) where
  showsPrec = I.showsPrec

instance (Arity (Peano n), Prim a, NFData a) => NFData (Vec n a) where
  rnf = foldl (\r a -> r `seq` rnf a) ()
  {-# INLINE rnf #-}

type instance Mutable (Vec n) = MVec n

instance (Arity (Peano n), Prim a) => MVector (MVec n) a where
  new = do
    v <- newByteArray $! peanoToInt (proxy# @(Peano n))
                       * sizeOf (undefined :: a)
    return $ MVec v
  {-# INLINE new         #-}
  copy                       = move
  {-# INLINE copy        #-}
  move (MVec dst) (MVec src) = copyMutableByteArray dst 0 src 0 (peanoToInt (proxy# @(Peano n)))
  {-# INLINE move        #-}
  unsafeRead  (MVec v) i   = readByteArray  v i
  {-# INLINE unsafeRead  #-}
  unsafeWrite (MVec v) i x = writeByteArray v i x
  {-# INLINE unsafeWrite #-}

instance (Arity (Peano n), Prim a) => IVector (Vec n) a where
  unsafeFreeze (MVec v)   = do { a <- unsafeFreezeByteArray v; return $! Vec  a }
  unsafeThaw   (Vec  v)   = do { a <- unsafeThawByteArray   v; return $! MVec a }
  unsafeIndex  (Vec  v) i = indexByteArray v i
  {-# INLINE unsafeFreeze #-}
  {-# INLINE unsafeThaw   #-}
  {-# INLINE unsafeIndex  #-}



type instance Dim  (Vec  n) = Peano n
type instance DimM (MVec n) = Peano n

instance (Arity (Peano n), Prim a) => Vector (Vec n) a where
  construct  = constructVec
  inspect    = inspectVec
  basicIndex = index
  {-# INLINE construct  #-}
  {-# INLINE inspect    #-}
  {-# INLINE basicIndex #-}
instance (Arity (Peano n), Prim a) => VectorN Vec n a

instance (Arity (Peano n), Prim a, Eq a) => Eq (Vec n a) where
  (==) = eq
  {-# INLINE (==) #-}
instance (Arity (Peano n), Prim a, Ord a) => Ord (Vec n a) where
  compare = ord
  {-# INLINE compare #-}

instance (Arity (Peano n), Prim a, Monoid a) => Monoid (Vec n a) where
  mempty  = replicate mempty
  mappend = (<>)
  {-# INLINE mempty  #-}
  {-# INLINE mappend #-}

instance (Arity (Peano n), Prim a, Semigroup a) => Semigroup (Vec n a) where
  (<>) = zipWith (<>)
  {-# INLINE (<>) #-}


instance (Typeable n, Arity (Peano n), Prim a, Data a) => Data (Vec n a) where
  gfoldl       = C.gfoldl
  gunfold      = C.gunfold
  toConstr   _ = con_Vec
  dataTypeOf _ = ty_Vec

ty_Vec :: DataType
ty_Vec  = mkDataType "Data.Vector.Fixed.Primitive.Vec" [con_Vec]

con_Vec :: Constr
con_Vec = mkConstr ty_Vec "Vec" [] Prefix

instance (Foreign.Storable a, Prim a, Arity (Peano n)) => Foreign.Storable (Vec n a) where
  alignment = defaultAlignemnt
  sizeOf    = defaultSizeOf
  peek      = defaultPeek
  poke      = defaultPoke
  {-# INLINE alignment #-}
  {-# INLINE sizeOf    #-}
  {-# INLINE peek      #-}
  {-# INLINE poke      #-}
