{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE UndecidableInstances #-}
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
import Data.Kind                (Type)
import Foreign.Storable         (Storable)
import GHC.TypeLits
import GHC.Exts (proxy#)
import Prelude (Show(..),Eq(..),Ord(..),Num(..))
import Prelude (($),($!),undefined,seq,(<$>))


import Data.Vector.Fixed hiding (index)
import Data.Vector.Fixed.Mutable (Mutable, MVector(..), IVector(..), DimM, constructVec, inspectVec, index)
import qualified Data.Vector.Fixed.Cont     as C
import           Data.Vector.Fixed.Cont     (ArityPeano(..))


----------------------------------------------------------------
-- Data type
----------------------------------------------------------------

-- | Unboxed vector with fixed length
newtype Vec (n :: Nat) (a :: Type) = Vec ByteArray

-- | Mutable unboxed vector with fixed length
newtype MVec (n :: Nat) s a = MVec (MutableByteArray s)

type Vec1 = Vec 1
type Vec2 = Vec 2
type Vec3 = Vec 3
type Vec4 = Vec 4
type Vec5 = Vec 5

type instance Mutable (Vec  n) = MVec n
type instance Dim     (Vec  n) = Peano n
type instance DimM    (MVec n) = Peano n


----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

instance (Arity n, Prim a, NFData a) => NFData (Vec n a) where
  rnf x = seq x ()

deriving via ViaFixed (Vec n) a instance (Arity n, Prim a, Show      a) => Show      (Vec n a)
deriving via ViaFixed (Vec n) a instance (Arity n, Prim a, Eq        a) => Eq        (Vec n a)
deriving via ViaFixed (Vec n) a instance (Arity n, Prim a, Ord       a) => Ord       (Vec n a)
deriving via ViaFixed (Vec n) a instance (Arity n, Prim a, Semigroup a) => Semigroup (Vec n a)
deriving via ViaFixed (Vec n) a instance (Arity n, Prim a, Monoid    a) => Monoid    (Vec n a)
deriving via ViaFixed (Vec n) a instance (Arity n, Prim a, Storable  a) => Storable  (Vec n a)
deriving via ViaFixed (Vec n) a instance (Arity n, Prim a)              => Prim      (Vec n a)

instance (Arity n, Prim a) => MVector (MVec n) a where
  basicNew = do
    v <- newByteArray $! peanoToInt (proxy# @(Peano n))
                       * sizeOf (undefined :: a)
    return $ MVec v
  {-# INLINE basicNew         #-}
  basicCopy (MVec dst) (MVec src) = copyMutableByteArray dst 0 src 0 (peanoToInt (proxy# @(Peano n)))
  {-# INLINE basicCopy        #-}
  basicUnsafeRead  (MVec v) i   = readByteArray  v i
  {-# INLINE basicUnsafeRead  #-}
  basicUnsafeWrite (MVec v) i x = writeByteArray v i x
  {-# INLINE basicUnsafeWrite #-}

instance (Arity n, Prim a) => IVector (Vec n) a where
  basicUnsafeFreeze (MVec v) = do { a <- unsafeFreezeByteArray v; return $! Vec  a }
  basicThaw         (Vec  v) = MVec <$> thawByteArray v 0 (peanoToInt (proxy# @(Peano n)))
  unsafeIndex       (Vec  v) i = indexByteArray v i
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicThaw         #-}
  {-# INLINE unsafeIndex       #-}

instance (Arity n, Prim a) => Vector (Vec n) a where
  construct  = constructVec
  inspect    = inspectVec
  basicIndex = index
  {-# INLINE construct  #-}
  {-# INLINE inspect    #-}
  {-# INLINE basicIndex #-}

instance (Typeable n, Arity n, Prim a, Data a) => Data (Vec n a) where
  gfoldl       = C.gfoldl
  gunfold      = C.gunfold
  toConstr   _ = con_Vec
  dataTypeOf _ = ty_Vec

ty_Vec :: DataType
ty_Vec  = mkDataType "Data.Vector.Fixed.Primitive.Vec" [con_Vec]

con_Vec :: Constr
con_Vec = mkConstr ty_Vec "Vec" [] Prefix
