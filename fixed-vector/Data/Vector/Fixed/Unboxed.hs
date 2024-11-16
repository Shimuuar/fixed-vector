{-# LANGUAGE CPP                  #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Unboxed vectors with fixed length.
module Data.Vector.Fixed.Unboxed(
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
  , Unbox
  ) where

import Control.Applicative   (Const(..))
import Control.Monad
import Control.DeepSeq       (NFData(..))
import Data.Complex
import Data.Data
import Data.Functor.Identity (Identity(..))
import Data.Int              (Int8, Int16, Int32, Int64 )
import Data.Monoid           (Monoid(..),Dual(..),Sum(..),Product(..),All(..),Any(..))
import Data.Semigroup        (Semigroup(..))
import Data.Ord              (Down(..))
import Data.Word             (Word,Word8,Word16,Word32,Word64)
import Foreign.Storable      (Storable(..))
import GHC.TypeLits
import Prelude               ( Show(..),Eq(..),Ord(..),Int,Double,Float,Char,Bool(..)
                             , ($),(.))

import Data.Vector.Fixed (Dim,Vector(..),ViaFixed(..))
import Data.Vector.Fixed.Mutable (Mutable, MVector(..), IVector(..), DimM, constructVec, inspectVec, Arity, index)
import qualified Data.Vector.Fixed.Cont      as C
import           Data.Vector.Fixed.Cont      (Peano)
import qualified Data.Vector.Fixed.Primitive as P


----------------------------------------------------------------
-- Data type
----------------------------------------------------------------

data family Vec  (n :: Nat) a
data family MVec (n :: Nat) s a

type Vec1 = Vec 1
type Vec2 = Vec 2
type Vec3 = Vec 3
type Vec4 = Vec 4
type Vec5 = Vec 5

class (Arity n, IVector (Vec n) a, MVector (MVec n) a) => Unbox n a

type instance Mutable (Vec  n) = MVec n
type instance Dim     (Vec  n) = Peano n
type instance DimM    (MVec n) = Peano n


----------------------------------------------------------------
-- Generic instances
----------------------------------------------------------------

deriving via ViaFixed (Vec n) a instance (Arity n, Unbox n a, Show      a) => Show      (Vec n a)
deriving via ViaFixed (Vec n) a instance (Arity n, Unbox n a, Eq        a) => Eq        (Vec n a)
deriving via ViaFixed (Vec n) a instance (Arity n, Unbox n a, Ord       a) => Ord       (Vec n a)
deriving via ViaFixed (Vec n) a instance (Arity n, Unbox n a, NFData    a) => NFData    (Vec n a)
deriving via ViaFixed (Vec n) a instance (Arity n, Unbox n a, Semigroup a) => Semigroup (Vec n a)
deriving via ViaFixed (Vec n) a instance (Arity n, Unbox n a, Monoid    a) => Monoid    (Vec n a)
deriving via ViaFixed (Vec n) a instance (Arity n, Unbox n a, Storable  a) => Storable  (Vec n a)

instance (Unbox n a) => Vector (Vec n) a where
  construct  = constructVec
  inspect    = inspectVec
  basicIndex = index
  {-# INLINE construct  #-}
  {-# INLINE inspect    #-}
  {-# INLINE basicIndex #-}

instance (Typeable n, Unbox n a, Data a) => Data (Vec n a) where
  gfoldl       = C.gfoldl
  gunfold      = C.gunfold
  toConstr   _ = con_Vec
  dataTypeOf _ = ty_Vec

ty_Vec :: DataType
ty_Vec  = mkDataType "Data.Vector.Fixed.Unboxed.Vec" [con_Vec]

con_Vec :: Constr
con_Vec = mkConstr ty_Vec "Vec" [] Prefix


----------------------------------------------------------------
-- Data instances
----------------------------------------------------------------

-- Unit type
data instance MVec n s () = MV_Unit
data instance Vec  n   () = V_Unit

instance Arity n => Unbox n ()

instance Arity n => MVector (MVec n) () where
  basicNew          = return MV_Unit
  {-# INLINE basicNew         #-}
  basicCopy _ _     = return ()
  {-# INLINE basicCopy        #-}
  basicUnsafeRead  _ _   = return ()
  {-# INLINE basicUnsafeRead  #-}
  basicUnsafeWrite _ _ _ = return ()
  {-# INLINE basicUnsafeWrite #-}

instance Arity n => IVector (Vec n) () where
  basicUnsafeFreeze _   = return V_Unit
  basicThaw   _   = return MV_Unit
  unsafeIndex  _ _ = ()
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicThaw   #-}
  {-# INLINE unsafeIndex  #-}



----------------------------------------------------------------
-- Boolean

newtype instance MVec n s Bool = MV_Bool (P.MVec n s Word8)
newtype instance Vec  n   Bool = V_Bool  (P.Vec  n   Word8)

instance Arity n => Unbox n Bool

instance Arity n => MVector (MVec n) Bool where
  basicNew          = MV_Bool `liftM` basicNew
  {-# INLINE basicNew         #-}
  basicCopy (MV_Bool v) (MV_Bool w) = basicCopy v w
  {-# INLINE basicCopy        #-}
  basicUnsafeRead  (MV_Bool v) i   = toBool `liftM` basicUnsafeRead v i
  {-# INLINE basicUnsafeRead  #-}
  basicUnsafeWrite (MV_Bool v) i b = basicUnsafeWrite v i (fromBool b)
  {-# INLINE basicUnsafeWrite #-}

instance Arity n => IVector (Vec n) Bool where
  basicUnsafeFreeze (MV_Bool v) = V_Bool  `liftM` basicUnsafeFreeze v
  basicThaw   (V_Bool  v) = MV_Bool `liftM` basicThaw   v
  unsafeIndex  (V_Bool  v) = toBool . unsafeIndex v
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicThaw   #-}
  {-# INLINE unsafeIndex  #-}


fromBool :: Bool -> Word8
{-# INLINE fromBool #-}
fromBool True = 1
fromBool False = 0

toBool :: Word8 -> Bool
{-# INLINE toBool #-}
toBool 0 = False
toBool _ = True


----------------------------------------------------------------
-- Primitive wrappers
#define primMV(ty,con)                              \
instance Arity n => MVector (MVec n) ty where {     \
; basicNew = con `liftM` basicNew                             \
; basicCopy (con v) (con w) = basicCopy v w                   \
; basicUnsafeRead  (con v) i = basicUnsafeRead v i            \
; basicUnsafeWrite (con v) i x = basicUnsafeWrite v i x       \
; {-# INLINE basicNew         #-}                        \
; {-# INLINE basicCopy        #-}                        \
; {-# INLINE basicUnsafeRead  #-}                        \
; {-# INLINE basicUnsafeWrite #-}                        \
}

#define primIV(ty,con,mcon)                             \
instance Arity n => IVector (Vec n) ty where {          \
; basicUnsafeFreeze (mcon v)   = con  `liftM` basicUnsafeFreeze v \
; basicThaw   (con  v)   = mcon `liftM` basicThaw   v \
; unsafeIndex  (con  v) i = unsafeIndex v i             \
; {-# INLINE basicUnsafeFreeze #-}                           \
; {-# INLINE basicThaw   #-}                           \
; {-# INLINE unsafeIndex  #-}                           \
}

#define primWrap(ty,con,mcon) \
newtype instance MVec n s ty = mcon (P.MVec n s ty) ; \
newtype instance Vec  n   ty = con  (P.Vec  n   ty) ; \
instance Arity n => Unbox n ty ; \
primMV(ty, mcon     )          ; \
primIV(ty, con, mcon)



primWrap(Int,   V_Int,   MV_Int  )
primWrap(Int8,  V_Int8,  MV_Int8 )
primWrap(Int16, V_Int16, MV_Int16)
primWrap(Int32, V_Int32, MV_Int32)
primWrap(Int64, V_Int64, MV_Int64)

primWrap(Word,   V_Word,   MV_Word  )
primWrap(Word8,  V_Word8,  MV_Word8 )
primWrap(Word16, V_Word16, MV_Word16)
primWrap(Word32, V_Word32, MV_Word32)
primWrap(Word64, V_Word64, MV_Word64)

primWrap(Char,   V_Char,   MV_Char  )
primWrap(Float,  V_Float,  MV_Float )
primWrap(Double, V_Double, MV_Double)



----------------------------------------------------------------
-- Complex
newtype instance MVec n s (Complex a) = MV_Complex (MVec n s (a,a))
newtype instance Vec  n   (Complex a) = V_Complex  (Vec  n   (a,a))

instance (Unbox n a) => Unbox n (Complex a)

instance (Arity n, MVector (MVec n) a) => MVector (MVec n) (Complex a) where
  basicNew = MV_Complex `liftM` basicNew
  {-# INLINE basicNew #-}
  basicCopy (MV_Complex v) (MV_Complex w) = basicCopy v w
  {-# INLINE basicCopy        #-}
  basicUnsafeRead (MV_Complex v) i = do (a,b) <- basicUnsafeRead v i
                                        return (a :+ b)
  {-# INLINE basicUnsafeRead  #-}
  basicUnsafeWrite (MV_Complex v) i (a :+ b) = basicUnsafeWrite v i (a,b)
  {-# INLINE basicUnsafeWrite #-}

instance (Arity n, IVector (Vec n) a) => IVector (Vec n) (Complex a) where
  basicUnsafeFreeze (MV_Complex v) = V_Complex `liftM` basicUnsafeFreeze v
  {-# INLINE basicUnsafeFreeze #-}
  basicThaw   (V_Complex  v) = MV_Complex `liftM` basicThaw v
  {-# INLINE basicThaw   #-}
  unsafeIndex (V_Complex v) i =
    case unsafeIndex v i of (a,b) -> a :+ b
  {-# INLINE unsafeIndex  #-}



----------------------------------------------------------------
-- Tuples
data instance MVec n s (a,b) = MV_2 !(MVec n s a) !(MVec n s b)
data instance Vec  n   (a,b) = V_2  !(Vec  n   a) !(Vec  n   b)

instance (Unbox n a, Unbox n b) => Unbox n (a,b)

instance (Arity n, MVector (MVec n) a, MVector (MVec n) b) => MVector (MVec n) (a,b) where
  basicNew = do as <- basicNew
                bs <- basicNew
                return $ MV_2 as bs
  {-# INLINE basicNew #-}
  basicCopy (MV_2 va vb) (MV_2 wa wb) = basicCopy va wa >> basicCopy vb wb
  {-# INLINE basicCopy        #-}
  basicUnsafeRead  (MV_2 v w) i = do a <- basicUnsafeRead v i
                                     b <- basicUnsafeRead w i
                                     return (a,b)
  {-# INLINE basicUnsafeRead  #-}
  basicUnsafeWrite (MV_2 v w) i (a,b) = basicUnsafeWrite v i a >> basicUnsafeWrite w i b
  {-# INLINE basicUnsafeWrite #-}


instance ( Arity n
         , IVector (Vec n) a, IVector (Vec n) b
         ) => IVector (Vec n) (a,b) where
  basicUnsafeFreeze (MV_2 v w)   = do as <- basicUnsafeFreeze v
                                      bs <- basicUnsafeFreeze w
                                      return $ V_2 as bs
  {-# INLINE basicUnsafeFreeze #-}
  basicThaw   (V_2  v w)   = do as <- basicThaw v
                                bs <- basicThaw w
                                return $ MV_2 as bs
  {-# INLINE basicThaw   #-}
  unsafeIndex  (V_2  v w) i = (unsafeIndex v i, unsafeIndex w i)
  {-# INLINE unsafeIndex  #-}




data instance MVec n s (a,b,c) = MV_3 !(MVec n s a) !(MVec n s b) !(MVec n s c)
data instance Vec  n   (a,b,c) = V_3  !(Vec  n   a) !(Vec  n   b) !(Vec  n   c)

instance (Unbox n a, Unbox n b, Unbox n c) => Unbox n (a,b,c)

instance (Arity n, MVector (MVec n) a, MVector (MVec n) b, MVector (MVec n) c
         ) => MVector (MVec n) (a,b,c) where
  basicNew = do as <- basicNew
                bs <- basicNew
                cs <- basicNew
                return $ MV_3 as bs cs
  {-# INLINE basicNew #-}
  basicCopy (MV_3 va vb vc) (MV_3 wa wb wc)
    = basicCopy va wa >> basicCopy vb wb >> basicCopy vc wc
  {-# INLINE basicCopy        #-}
  basicUnsafeRead  (MV_3 v w u) i = do a <- basicUnsafeRead v i
                                       b <- basicUnsafeRead w i
                                       c <- basicUnsafeRead u i
                                       return (a,b,c)
  {-# INLINE basicUnsafeRead  #-}
  basicUnsafeWrite (MV_3 v w u) i (a,b,c)
    = basicUnsafeWrite v i a >> basicUnsafeWrite w i b >> basicUnsafeWrite u i c
  {-# INLINE basicUnsafeWrite #-}

instance ( Arity n
         , Vector  (Vec n) a, Vector  (Vec n) b, Vector  (Vec n) c
         , IVector (Vec n) a, IVector (Vec n) b, IVector (Vec n) c
         ) => IVector (Vec n) (a,b,c) where
  basicUnsafeFreeze (MV_3 v w u) = do as <- basicUnsafeFreeze v
                                      bs <- basicUnsafeFreeze w
                                      cs <- basicUnsafeFreeze u
                                      return $ V_3 as bs cs
  {-# INLINE basicUnsafeFreeze #-}
  basicThaw   (V_3  v w u) = do as <- basicThaw v
                                bs <- basicThaw w
                                cs <- basicThaw u
                                return $ MV_3 as bs cs
  {-# INLINE basicThaw   #-}
  unsafeIndex  (V_3 v w u) i
    = (unsafeIndex v i, unsafeIndex w i, unsafeIndex u i)
  {-# INLINE unsafeIndex  #-}


----------------------------------------------------------------
-- Newtype wrappers

newtype instance MVec n s (Const a b) = MV_Const (MVec n s a)
newtype instance Vec  n   (Const a b) = V_Const  (Vec  n   a)
instance Unbox n a => Unbox n (Const a b)
deriving newtype instance (Unbox n a) => MVector (MVec n) (Const a b)
deriving newtype instance (Unbox n a) => IVector (Vec n)  (Const a b)


----------------------------------------------------------------
-- Newtype wrappers with kind * -> *

#define primNewMV(ty,con)                         \
instance Unbox n a => MVector (MVec n) (ty a) where {     \
; basicNew = con `liftM` basicNew                             \
; basicCopy (con v) (con w) = basicCopy v w                   \
; basicUnsafeRead  (con v) i = ty `liftM` basicUnsafeRead v i            \
; basicUnsafeWrite (con v) i (ty x) = basicUnsafeWrite v i x       \
; {-# INLINE basicNew         #-}                        \
; {-# INLINE basicCopy        #-}                        \
; {-# INLINE basicUnsafeRead  #-}                        \
; {-# INLINE basicUnsafeWrite #-}                        \
}

#define primNewIV(ty,con,mcon)                             \
instance Unbox n a => IVector (Vec n) (ty a)  where {          \
; basicUnsafeFreeze (mcon v)   = con  `liftM` basicUnsafeFreeze v \
; basicThaw   (con  v)   = mcon `liftM` basicThaw   v \
; unsafeIndex  (con  v) i = ty (unsafeIndex v i)             \
; {-# INLINE basicUnsafeFreeze #-}                           \
; {-# INLINE basicThaw   #-}                           \
; {-# INLINE unsafeIndex  #-}                           \
}

#define primNewWrap(ty,con,mcon) \
newtype instance MVec n s (ty a) = mcon (MVec n s a) ; \
newtype instance Vec  n   (ty a) = con  (Vec  n   a) ; \
instance Unbox n a => Unbox n (ty a) ; \
primNewMV(ty, mcon     )          ; \
primNewIV(ty, con, mcon)


primNewWrap(Identity, V_Identity, MV_Identity)
primNewWrap(Down, V_Down, MV_Down)
primNewWrap(Dual, V_Dual, MV_Dual)
primNewWrap(Sum, V_Sum, MV_Sum)
primNewWrap(Product, V_Product, MV_Product)


----------------------------------------------------------------
-- Monomorphic newtype wrappers


newtype instance MVec n s Any = MV_Any (MVec n s Bool)
newtype instance Vec  n   Any = V_Any  (Vec  n   Bool)
instance Arity n => Unbox n Any
deriving newtype instance Arity n => IVector (Vec  n) Any
deriving newtype instance Arity n => MVector (MVec n) Any

newtype instance MVec n s All = MV_All (MVec n s Bool)
newtype instance Vec  n   All = V_All  (Vec  n   Bool)
instance Arity n => Unbox n All
deriving newtype instance Arity n => IVector (Vec  n) All
deriving newtype instance Arity n => MVector (MVec n) All
