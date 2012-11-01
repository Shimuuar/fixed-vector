{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Data.Vector.Fixed.Storable (
    -- * Immutable
    Vec
  , Vec2
  , Vec3
    -- * Mutable
  , MVec
  ) where

import Control.Monad.Primitive
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.Marshal.Array ( advancePtr, copyArray, moveArray )
import GHC.ForeignPtr        ( ForeignPtr(..), mallocPlainForeignPtrBytes )
import GHC.Ptr               ( Ptr(..) )

import Prelude hiding (length,replicate,zipWith,map,foldl)

import Data.Vector.Fixed
import Data.Vector.Fixed.Mutable



----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Storable-based vector with fixed length
newtype Vec n a = Vec (ForeignPtr a)

-- | Storable-based mutable vector with fixed length
newtype MVec n s a = MVec (ForeignPtr a)

type Vec2 = Vec (S (S Z))
type Vec3 = Vec (S (S (S Z)))


----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

instance (Arity n, Storable a, Show a) => Show (Vec n a) where
  show v = "fromList " ++ show (toList v)



type instance Mutable (Vec n) = MVec n

instance (Arity n, Storable a) => MVector (MVec n) a where
  lengthM _ = arity (undefined :: n)
  {-# INLINE lengthM     #-}
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


type instance Dim (Vec  n  ) = n
type instance Dim (MVec n s) = n

instance (Arity n, Storable a) => Vector (Vec n) a where
  construct = constructVec
  inspect   = inspectVec
  {-# INLINE construct #-}
  {-# INLINE inspect   #-}



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
