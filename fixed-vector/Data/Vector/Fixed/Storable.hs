{-# LANGUAGE CPP                   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
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
import Data.Monoid           (Monoid(..))
import Data.Data
import Foreign.Ptr           (castPtr)
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Marshal.Array ( advancePtr, copyArray, moveArray )
import GHC.ForeignPtr        ( ForeignPtr(..), mallocPlainForeignPtrBytes )
import GHC.Ptr               ( Ptr(..) )
import Prelude (Show(..),Eq(..),Ord(..),Num(..),Monad(..),IO,Int)
import Prelude ((++),(&&),(||),($),undefined)

import Data.Vector.Fixed hiding (index)
import Data.Vector.Fixed.Mutable
import qualified Data.Vector.Fixed.Cont as C



----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Storable-based vector with fixed length
newtype Vec n a = Vec (ForeignPtr a)

-- | Storable-based mutable vector with fixed length
newtype MVec n s a = MVec (ForeignPtr a)

#if __GLASGOW_HASKELL__ >= 708
deriving instance Typeable Vec
deriving instance Typeable MVec
#else
deriving instance Typeable2 Vec
deriving instance Typeable3 MVec
#endif

type Vec1 = Vec (S Z)
type Vec2 = Vec (S (S Z))
type Vec3 = Vec (S (S (S Z)))
type Vec4 = Vec (S (S (S (S Z))))
type Vec5 = Vec (S (S (S (S (S Z)))))



----------------------------------------------------------------
-- Raw Ptrs
----------------------------------------------------------------

-- | Get underlying pointer. Data may not be modified through pointer.
unsafeToForeignPtr :: Storable a => Vec n a -> ForeignPtr a
{-# INLINE unsafeToForeignPtr #-}
unsafeToForeignPtr (Vec fp) = fp

-- | Construct vector from foreign pointer.
unsafeFromForeignPtr :: Storable a => ForeignPtr a -> Vec n a
{-# INLINE unsafeFromForeignPtr #-}
unsafeFromForeignPtr = Vec

unsafeWith :: Storable a => (Ptr a -> IO b) -> Vec n a -> IO b
{-# INLINE unsafeWith #-}
unsafeWith f (Vec fp) = f (getPtr fp)



----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

instance (Arity n, Storable a, Show a) => Show (Vec n a) where
  show v = "fromList " ++ show (toList v)



type instance Mutable (Vec n) = MVec n

instance (Arity n, Storable a) => MVector (MVec n) a where
  overlaps (MVec fp) (MVec fq)
    = between p q (q `advancePtr` n) || between q p (p `advancePtr` n)
    where
      between x y z = x >= y && x < z
      p = getPtr fp
      q = getPtr fq
      n = arity (undefined :: n)
  {-# INLINE overlaps    #-}
  new = unsafePrimToPrim $ do
    fp <- mallocVector $ arity (undefined :: n)
    return $ MVec fp
  {-# INLINE new         #-}
  copy (MVec fp) (MVec fq)
    = unsafePrimToPrim
    $ withForeignPtr fp $ \p ->
      withForeignPtr fq $ \q ->
      copyArray p q (arity (undefined :: n))
  {-# INLINE copy        #-}
  move (MVec fp) (MVec fq)
    = unsafePrimToPrim
    $ withForeignPtr fp $ \p ->
      withForeignPtr fq $ \q ->
      moveArray p q (arity (undefined :: n))
  {-# INLINE move        #-}
  unsafeRead (MVec fp) i
    = unsafePrimToPrim
    $ withForeignPtr fp (`peekElemOff` i)
  {-# INLINE unsafeRead  #-}
  unsafeWrite (MVec fp) i x
    = unsafePrimToPrim
    $ withForeignPtr fp $ \p -> pokeElemOff p i x
  {-# INLINE unsafeWrite #-}


instance (Arity n, Storable a) => IVector (Vec n) a where
  unsafeFreeze (MVec fp)   = return $ Vec  fp
  unsafeThaw   (Vec  fp)   = return $ MVec fp
  unsafeIndex  (Vec  fp) i
    = unsafeInlineIO
    $ withForeignPtr fp (`peekElemOff` i)
  {-# INLINE unsafeFreeze #-}
  {-# INLINE unsafeThaw   #-}
  {-# INLINE unsafeIndex  #-}


type instance Dim  (Vec  n) = n
type instance DimM (MVec n) = n

instance (Arity n, Storable a) => Vector (Vec n) a where
  construct  = constructVec
  inspect    = inspectVec
  basicIndex = index
  {-# INLINE construct  #-}
  {-# INLINE inspect    #-}
  {-# INLINE basicIndex #-}
instance (Arity n, Storable a) => VectorN Vec n a

instance (Arity n, Storable a, Eq a) => Eq (Vec n a) where
  (==) = eq
  {-# INLINE (==) #-}
instance (Arity n, Storable a, Ord a) => Ord (Vec n a) where
  compare = ord
  {-# INLINE compare #-}

instance (Arity n, Storable a, Monoid a) => Monoid (Vec n a) where
  mempty  = replicate mempty
  mappend = zipWith mappend
  {-# INLINE mempty  #-}
  {-# INLINE mappend #-}

instance (Arity n, Storable a) => Storable (Vec n a) where
  sizeOf    _ = arity (undefined :: n) * sizeOf (undefined :: a)
  alignment _ = alignment (undefined :: a)
  peek ptr = do
    arr@(MVec fp) <- new
    withForeignPtr fp $ \p ->
      moveArray p (castPtr ptr) (arity (undefined :: n))
    unsafeFreeze arr
  poke ptr (Vec fp)
    = withForeignPtr fp $ \p ->
      moveArray (castPtr ptr) p (arity (undefined :: n))

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

getPtr :: ForeignPtr a -> Ptr a
{-# INLINE getPtr #-}
getPtr (ForeignPtr addr _) = Ptr addr
