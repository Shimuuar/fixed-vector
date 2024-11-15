{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE UndecidableInstances #-}
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
import GHC.Exts (proxy#)
import Prelude ( Show(..),Eq(..),Ord(..),Functor(..),Monad(..)
               , ($),($!),error)

import Data.Vector.Fixed hiding (index)
import Data.Vector.Fixed.Mutable (Mutable, MVector(..), IVector(..), DimM, constructVec, inspectVec, index)
import qualified Data.Vector.Fixed.Cont     as C
import           Data.Vector.Fixed.Cont     (Peano,ArityPeano(..))



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

type instance Mutable (Vec  n) = MVec n
type instance Dim     (Vec  n) = Peano n
type instance DimM    (MVec n) = Peano n



instance (Typeable n, Arity n, Data a) => Data (Vec n a) where
  gfoldl       = C.gfoldl
  gunfold      = C.gunfold
  toConstr   _ = con_Vec
  dataTypeOf _ = ty_Vec

ty_Vec :: DataType
ty_Vec  = mkDataType "Data.Vector.Fixed.Boxed.Vec" [con_Vec]

con_Vec :: Constr
con_Vec = mkConstr ty_Vec "Vec" [] Prefix


----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

deriving via ViaFixed (Vec n) instance Arity n => Functor    (Vec n)
deriving via ViaFixed (Vec n) instance Arity n => Applicative (Vec n)
deriving via ViaFixed (Vec n) instance Arity n => F.Foldable  (Vec n)

instance Arity n => T.Traversable (Vec n) where
  sequenceA = sequenceA
  traverse  = traverse
  {-# INLINE sequenceA #-}
  {-# INLINE traverse #-}

deriving via ViaFixed (Vec n) a instance (Arity n, Show      a) => Show      (Vec n a)
deriving via ViaFixed (Vec n) a instance (Arity n, Eq        a) => Eq        (Vec n a)
deriving via ViaFixed (Vec n) a instance (Arity n, Ord       a) => Ord       (Vec n a)
deriving via ViaFixed (Vec n) a instance (Arity n, NFData    a) => NFData    (Vec n a)
deriving via ViaFixed (Vec n) a instance (Arity n, Semigroup a) => Semigroup (Vec n a)
deriving via ViaFixed (Vec n) a instance (Arity n, Monoid    a) => Monoid    (Vec n a)
deriving via ViaFixed (Vec n) a instance (Arity n, Storable  a) => Storable  (Vec n a)

instance (Arity n) => MVector (MVec n) a where
  new = do
    v <- newSmallArray (peanoToInt (proxy# @(Peano n))) uninitialised
    return $ MVec v
  {-# INLINE new         #-}
  copy = move
  {-# INLINE copy        #-}
  move (MVec dst) (MVec src) = copySmallMutableArray dst 0 src 0 (peanoToInt (proxy# @(Peano n)))
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

instance (Arity n) => Vector (Vec n) a where
  construct  = constructVec
  inspect    = inspectVec
  basicIndex = index
  {-# INLINE construct  #-}
  {-# INLINE inspect    #-}
  {-# INLINE basicIndex #-}


uninitialised :: a
uninitialised = error "Data.Vector.Fixed.Boxed: uninitialised element"
