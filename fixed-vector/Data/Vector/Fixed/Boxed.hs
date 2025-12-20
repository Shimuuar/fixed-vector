{-# LANGUAGE CPP                  #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE UnboxedTuples        #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Lazy vector which could hold any value. For strict variant see
-- "Data.Vector.Fixed.Strict".
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
import Data.Primitive.Types (Prim)
import qualified Data.Foldable    as F
#if MIN_VERSION_base(4,18,0)
import qualified Data.Foldable1   as F1
#endif
import qualified Data.Traversable as T
import Foreign.Storable (Storable)
import GHC.TypeLits
import GHC.Exts (proxy#)
import Prelude ( Show(..),Eq(..),Ord(..),Functor(..),Monad(..)
               , ($!),error,(<$>))

import Data.Vector.Fixed hiding (index)
import Data.Vector.Fixed.Mutable (Mutable, MVector(..), IVector(..), DimM, constructVec, inspectVec, index)
import qualified Data.Vector.Fixed.Cont     as C
import           Data.Vector.Fixed.Cont     (ArityPeano(..))


----------------------------------------------------------------
-- Data type
----------------------------------------------------------------

-- | Vector with fixed length which can hold any value. It's lazy and
--   doesn't evaluate elements.
newtype Vec (n :: Nat) a = Vec (SmallArray a)

-- | Mutable unboxed vector with fixed length
newtype MVec (n :: Nat) s a = MVec (SmallMutableArray s a)

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

deriving via ViaFixed (Vec n) instance Arity n => Functor    (Vec n)
deriving via ViaFixed (Vec n) instance Arity n => Applicative (Vec n)
deriving via ViaFixed (Vec n) instance Arity n => F.Foldable  (Vec n)
#if MIN_VERSION_base(4,18,0)
deriving via ViaFixed (Vec n)
    instance (Arity n, Peano n ~ S k) => F1.Foldable1 (Vec n)
#endif

instance Arity n => T.Traversable (Vec n) where
  sequence  = sequence
  sequenceA = sequence
  traverse  = mapM
  mapM      = mapM
  {-# INLINE sequence  #-}
  {-# INLINE sequenceA #-}
  {-# INLINE mapM      #-}
  {-# INLINE traverse  #-}

deriving via ViaFixed (Vec n) a instance (Arity n, Show      a) => Show      (Vec n a)
deriving via ViaFixed (Vec n) a instance (Arity n, Eq        a) => Eq        (Vec n a)
deriving via ViaFixed (Vec n) a instance (Arity n, Ord       a) => Ord       (Vec n a)
deriving via ViaFixed (Vec n) a instance (Arity n, NFData    a) => NFData    (Vec n a)
deriving via ViaFixed (Vec n) a instance (Arity n, Semigroup a) => Semigroup (Vec n a)
deriving via ViaFixed (Vec n) a instance (Arity n, Monoid    a) => Monoid    (Vec n a)
deriving via ViaFixed (Vec n) a instance (Arity n, Storable  a) => Storable  (Vec n a)
-- | @since 2.0.1.0
deriving via ViaFixed (Vec n) a instance (Arity n, Prim      a) => Prim      (Vec n a)

instance (Arity n) => MVector (MVec n) a where
  basicNew =
    MVec <$> newSmallArray (peanoToInt (proxy# @(Peano n))) uninitialised
  basicReplicate a =
    MVec <$> newSmallArray (peanoToInt (proxy# @(Peano n))) a
  basicCopy (MVec dst) (MVec src) =
    copySmallMutableArray dst 0 src 0 (peanoToInt (proxy# @(Peano n)))
  basicClone (MVec src) =
    MVec <$> cloneSmallMutableArray src 0 (peanoToInt (proxy# @(Peano n)))
  basicUnsafeRead  (MVec v) i   = readSmallArray  v i
  basicUnsafeWrite (MVec v) i x = writeSmallArray v i x
  {-# INLINE basicNew         #-}
  {-# INLINE basicReplicate   #-}
  {-# INLINE basicCopy        #-}
  {-# INLINE basicClone       #-}
  {-# INLINE basicUnsafeRead  #-}
  {-# INLINE basicUnsafeWrite #-}

instance (Arity n) => IVector (Vec n) a where
  basicUnsafeFreeze (MVec v) = do { a <- unsafeFreezeSmallArray v; return $! Vec  a }
  basicThaw         (Vec  v) =
    MVec <$> thawSmallArray v 0 (peanoToInt (proxy# @(Peano n)))
  unsafeIndex  (Vec  v) i = indexSmallArray v i
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicThaw         #-}
  {-# INLINE unsafeIndex       #-}

instance (Arity n) => Vector (Vec n) a where
  construct  = constructVec
  inspect    = inspectVec
  basicIndex = index
  {-# INLINE construct  #-}
  {-# INLINE inspect    #-}
  {-# INLINE basicIndex #-}

instance (Typeable n, Arity n, Data a) => Data (Vec n a) where
  gfoldl       = C.gfoldl
  gunfold      = C.gunfold
  toConstr   _ = con_Vec
  dataTypeOf _ = ty_Vec

ty_Vec :: DataType
ty_Vec  = mkDataType "Data.Vector.Fixed.Boxed.Vec" [con_Vec]

con_Vec :: Constr
con_Vec = mkConstr ty_Vec "Vec" [] Prefix

uninitialised :: a
uninitialised = error "Data.Vector.Fixed.Boxed: uninitialised element"
