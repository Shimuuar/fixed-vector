{-# LANGUAGE CPP                  #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Storable-based unboxed vectors.
module Data.Vector.Fixed.Storable (
    -- * Immutable
    Vec
  , Vec1
  , Vec2
  , Vec3
  , Vec4
  , Vec5
    -- * Raw pointers
  , unsafeFromForeignPtr
  , unsafeToForeignPtr
  , unsafeWith
    -- * Mutable
  , MVec(..)
    -- * Type classes
  , Storable
  ) where

import Control.Monad.Primitive
import Control.DeepSeq (NFData(..))
import Data.Monoid           (Monoid(..))
import Data.Semigroup        (Semigroup(..))
import Data.Data
import Data.Primitive.Types  (Prim)
import Foreign.Ptr           (castPtr)
import Foreign.Storable
import Foreign.Marshal.Array ( copyArray, moveArray )
import GHC.ForeignPtr        ( mallocPlainForeignPtrBytes )
import GHC.Ptr               ( Ptr(..) )
import GHC.Exts              ( proxy# )
import GHC.TypeLits
#if MIN_VERSION_base(4,15,0)
import GHC.ForeignPtr       ( unsafeWithForeignPtr )
#endif
import Foreign.ForeignPtr   ( ForeignPtr, withForeignPtr )
import Prelude ( Show(..),Eq(..),Ord(..),Num(..),Monad(..),IO,Int
               , ($),undefined,seq,pure)

import Data.Vector.Fixed hiding (index)
import Data.Vector.Fixed.Mutable (Mutable, MVector(..), IVector(..), DimM, constructVec, inspectVec, index, new,unsafeFreeze)
import qualified Data.Vector.Fixed.Cont     as C
import           Data.Vector.Fixed.Cont     (ArityPeano(..))



----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Storable-based vector with fixed length
newtype Vec (n :: Nat) a = Vec (ForeignPtr a)

-- | Storable-based mutable vector with fixed length
newtype MVec (n :: Nat) s a = MVec (ForeignPtr a)

type Vec1 = Vec 1
type Vec2 = Vec 2
type Vec3 = Vec 3
type Vec4 = Vec 4
type Vec5 = Vec 5

type instance Mutable (Vec  n) = MVec n
type instance Dim     (Vec  n) = Peano n
type instance DimM    (MVec n) = Peano n


----------------------------------------------------------------
-- Raw Ptrs
----------------------------------------------------------------

-- | Get underlying pointer. Data may not be modified through pointer.
unsafeToForeignPtr :: Vec n a -> ForeignPtr a
{-# INLINE unsafeToForeignPtr #-}
unsafeToForeignPtr (Vec fp) = fp

-- | Construct vector from foreign pointer.
unsafeFromForeignPtr :: ForeignPtr a -> Vec n a
{-# INLINE unsafeFromForeignPtr #-}
unsafeFromForeignPtr = Vec

-- | Pass pointer to the vector's data to the IO action. The data may
--   not be modified through the 'Ptr.
unsafeWith :: (Ptr a -> IO b) -> Vec n a -> IO b
{-# INLINE unsafeWith #-}
unsafeWith f (Vec fp) = withForeignPtr fp f



----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

instance (Arity n, Storable a, NFData a) => NFData (Vec n a) where
  rnf x = seq x ()

deriving via ViaFixed (Vec n) a instance (Arity n, Storable a, Show      a) => Show      (Vec n a)
deriving via ViaFixed (Vec n) a instance (Arity n, Storable a, Eq        a) => Eq        (Vec n a)
deriving via ViaFixed (Vec n) a instance (Arity n, Storable a, Ord       a) => Ord       (Vec n a)
deriving via ViaFixed (Vec n) a instance (Arity n, Storable a, Semigroup a) => Semigroup (Vec n a)
deriving via ViaFixed (Vec n) a instance (Arity n, Storable a, Monoid    a) => Monoid    (Vec n a)

instance (Arity n, Storable a) => MVector (MVec n) a where
  basicNew = unsafePrimToPrim $ do
    fp <- mallocVector (peanoToInt (proxy# @(Peano n)))
    return $ MVec fp
  {-# INLINE basicNew         #-}
  basicCopy (MVec fp) (MVec fq)
    = unsafePrimToPrim
    $ unsafeWithForeignPtr fp $ \p ->
      unsafeWithForeignPtr fq $ \q ->
      copyArray p q (peanoToInt (proxy# @(Peano n)))
  {-# INLINE basicCopy        #-}
  basicUnsafeRead (MVec fp) i
    = unsafePrimToPrim
    $ unsafeWithForeignPtr fp (`peekElemOff` i)
  {-# INLINE basicUnsafeRead  #-}
  basicUnsafeWrite (MVec fp) i x
    = unsafePrimToPrim
    $ unsafeWithForeignPtr fp $ \p -> pokeElemOff p i x
  {-# INLINE basicUnsafeWrite #-}

instance (Arity n, Storable a) => IVector (Vec n) a where
  basicUnsafeFreeze (MVec fp) = return $ Vec  fp
  basicThaw         (Vec  fp) = do
    mv <- basicNew
    basicCopy mv (MVec fp)
    pure mv
  unsafeIndex  (Vec  fp) i
    = unsafeInlineIO
    $ unsafeWithForeignPtr fp (`peekElemOff` i)
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicThaw         #-}
  {-# INLINE unsafeIndex       #-}

instance (Arity n, Storable a) => Vector (Vec n) a where
  construct  = constructVec
  inspect    = inspectVec
  basicIndex = index
  {-# INLINE construct  #-}
  {-# INLINE inspect    #-}
  {-# INLINE basicIndex #-}

instance (Arity n, Storable a) => Storable (Vec n a) where
  sizeOf    = defaultSizeOf
  alignment = defaultAlignemnt
  peek ptr = do
    arr@(MVec fp) <- new
    unsafeWithForeignPtr fp $ \p ->
      moveArray p (castPtr ptr) (peanoToInt (proxy# @(Peano n)))
    unsafeFreeze arr
  poke ptr (Vec fp)
    = unsafeWithForeignPtr fp $ \p ->
      moveArray (castPtr ptr) p (peanoToInt (proxy# @(Peano n)))

deriving via ViaFixed (Vec n) a instance (Arity n, Storable a, Prim a) => Prim (Vec n a)

instance (Typeable n, Arity n, Storable a, Data a) => Data (Vec n a) where
  gfoldl       = C.gfoldl
  gunfold      = C.gunfold
  toConstr   _ = con_Vec
  dataTypeOf _ = ty_Vec

ty_Vec :: DataType
ty_Vec  = mkDataType "Data.Vector.Fixed.Primitive.Vec" [con_Vec]

con_Vec :: Constr
con_Vec = mkConstr ty_Vec "Vec" [] Prefix




----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

-- Code copied verbatim from vector package

mallocVector :: forall a. Storable a => Int -> IO (ForeignPtr a)
{-# INLINE mallocVector #-}
mallocVector size
  = mallocPlainForeignPtrBytes (size * sizeOf (undefined :: a))

#if !MIN_VERSION_base(4,15,0)
-- | A compatibility wrapper for 'GHC.ForeignPtr.unsafeWithForeignPtr' provided
-- by GHC 9.0.1 and later.
--
-- Only to be used when the continuation is known not to
-- unconditionally diverge lest unsoundness can result.
unsafeWithForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
unsafeWithForeignPtr = withForeignPtr
#endif
