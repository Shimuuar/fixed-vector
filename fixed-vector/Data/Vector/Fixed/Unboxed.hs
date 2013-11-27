{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Unboxed vectors with fixed length.
module Data.Vector.Fixed.Unboxed(
    -- * Immutable
    Vec
  , Vec2
  , Vec3
  , Vec4
  , Vec5
    -- * Mutable
  , MVec
    -- * Type classes
  , Unbox
  ) where

import Control.Monad
import Data.Complex
import Data.Monoid     (Monoid(..))
import Data.Data
import Data.Int        (Int8, Int16, Int32, Int64 )
import Data.Word       (Word,Word8,Word16,Word32,Word64)
import Prelude hiding (length,replicate,zipWith,map,foldl)

import Data.Vector.Fixed (Dim,Vector(..),VectorN,S,Z,toList,eq,ord,replicate,zipWith)
import Data.Vector.Fixed.Mutable
import qualified Data.Vector.Fixed.Cont      as C
import qualified Data.Vector.Fixed.Primitive as P



----------------------------------------------------------------
-- Data type
----------------------------------------------------------------

data family Vec  n a
data family MVec n s a

deriving instance Typeable2 Vec
deriving instance Typeable3 MVec

type Vec2 = Vec (S (S Z))
type Vec3 = Vec (S (S (S Z)))
type Vec4 = Vec (S (S (S (S Z))))
type Vec5 = Vec (S (S (S (S (S Z)))))

class (IVector (Vec n) a, MVector (MVec n) a) => Unbox n a


----------------------------------------------------------------
-- Generic instances
----------------------------------------------------------------

instance (Arity n, Show a, Unbox n a) => Show (Vec n a) where
  show v = "fromList " ++ show (toList v)

type instance Mutable (Vec n) = MVec n

type instance Dim  (Vec  n) = n
type instance DimM (MVec n) = n

instance (Unbox n a) => Vector (Vec n) a where
  construct  = constructVec
  inspect    = inspectVec
  basicIndex = index
  {-# INLINE construct  #-}
  {-# INLINE inspect    #-}
  {-# INLINE basicIndex #-}


instance (Unbox n a) => VectorN Vec n a

instance (Unbox n a, Eq a) => Eq (Vec n a) where
  (==) = eq
  {-# INLINE (==) #-}
instance (Unbox n a, Ord a) => Ord (Vec n a) where
  compare = ord
  {-# INLINE compare #-}

instance (Unbox n a, Monoid a) => Monoid (Vec n a) where
  mempty  = replicate mempty
  mappend = zipWith mappend
  {-# INLINE mempty  #-}
  {-# INLINE mappend #-}

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
  overlaps _ _ = False
  {-# INLINE overlaps    #-}
  new          = return MV_Unit
  {-# INLINE new         #-}
  copy _ _     = return ()
  {-# INLINE move        #-}
  move _ _     = return ()
  {-# INLINE copy        #-}
  unsafeRead  _ _   = return ()
  {-# INLINE unsafeRead  #-}
  unsafeWrite _ _ _ = return ()
  {-# INLINE unsafeWrite #-}

instance Arity n => IVector (Vec n) () where
  unsafeFreeze _   = return V_Unit
  unsafeThaw   _   = return MV_Unit
  unsafeIndex  _ _ = ()
  {-# INLINE unsafeFreeze #-}
  {-# INLINE unsafeThaw   #-}
  {-# INLINE unsafeIndex  #-}



----------------------------------------------------------------
-- Boolean

newtype instance MVec n s Bool = MV_Bool (P.MVec n s Word8)
newtype instance Vec  n   Bool = V_Bool  (P.Vec  n   Word8)

instance Arity n => Unbox n Bool

instance Arity n => MVector (MVec n) Bool where
  overlaps (MV_Bool v) (MV_Bool w) = overlaps v w
  {-# INLINE overlaps    #-}
  new          = MV_Bool `liftM` new
  {-# INLINE new         #-}
  copy (MV_Bool v) (MV_Bool w) = copy v w
  {-# INLINE copy        #-}
  move (MV_Bool v) (MV_Bool w) = move v w
  {-# INLINE move        #-}
  unsafeRead  (MV_Bool v) i   = toBool `liftM` unsafeRead v i
  {-# INLINE unsafeRead  #-}
  unsafeWrite (MV_Bool v) i b = unsafeWrite v i (fromBool b)
  {-# INLINE unsafeWrite #-}

instance Arity n => IVector (Vec n) Bool where
  unsafeFreeze (MV_Bool v) = V_Bool  `liftM` unsafeFreeze v
  unsafeThaw   (V_Bool  v) = MV_Bool `liftM` unsafeThaw   v
  unsafeIndex  (V_Bool  v) = toBool . unsafeIndex v
  {-# INLINE unsafeFreeze #-}
  {-# INLINE unsafeThaw   #-}
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
; overlaps (con v) (con w) = overlaps v w           \
; new = con `liftM` new                             \
; copy (con v) (con w) = copy v w                   \
; move (con v) (con w) = move v w                   \
; unsafeRead  (con v) i = unsafeRead v i            \
; unsafeWrite (con v) i x = unsafeWrite v i x       \
; {-# INLINE overlaps    #-}                        \
; {-# INLINE new         #-}                        \
; {-# INLINE move        #-}                        \
; {-# INLINE copy        #-}                        \
; {-# INLINE unsafeRead  #-}                        \
; {-# INLINE unsafeWrite #-}                        \
}

#define primIV(ty,con,mcon)                             \
instance Arity n => IVector (Vec n) ty where {          \
; unsafeFreeze (mcon v)   = con  `liftM` unsafeFreeze v \
; unsafeThaw   (con  v)   = mcon `liftM` unsafeThaw   v \
; unsafeIndex  (con  v) i = unsafeIndex v i             \
; {-# INLINE unsafeFreeze #-}                           \
; {-# INLINE unsafeThaw   #-}                           \
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
  overlaps (MV_Complex v) (MV_Complex w) = overlaps v w
  {-# INLINE overlaps    #-}
  new = MV_Complex `liftM` new
  {-# INLINE new #-}
  copy (MV_Complex v) (MV_Complex w) = copy v w
  {-# INLINE copy        #-}
  move (MV_Complex v) (MV_Complex w) = move v w
  {-# INLINE move        #-}
  unsafeRead (MV_Complex v) i = do (a,b) <- unsafeRead v i
                                   return (a :+ b)
  {-# INLINE unsafeRead  #-}
  unsafeWrite (MV_Complex v) i (a :+ b) = unsafeWrite v i (a,b)
  {-# INLINE unsafeWrite #-}

instance (Arity n, IVector (Vec n) a) => IVector (Vec n) (Complex a) where
  unsafeFreeze (MV_Complex v) = V_Complex `liftM` unsafeFreeze v 
  {-# INLINE unsafeFreeze #-}
  unsafeThaw   (V_Complex  v) = MV_Complex `liftM` unsafeThaw v
  {-# INLINE unsafeThaw   #-}
  unsafeIndex (V_Complex v) i =
    case unsafeIndex v i of (a,b) -> a :+ b
  {-# INLINE unsafeIndex  #-}



----------------------------------------------------------------
-- Tuples
data instance MVec n s (a,b) = MV_2 !(MVec n s a) !(MVec n s b)
data instance Vec  n   (a,b) = V_2  !(Vec  n   a) !(Vec  n   b)

instance (Unbox n a, Unbox n b) => Unbox n (a,b)

instance (Arity n, MVector (MVec n) a, MVector (MVec n) b) => MVector (MVec n) (a,b) where
  overlaps (MV_2 va vb) (MV_2 wa wb) = overlaps va wa || overlaps vb wb
  {-# INLINE overlaps    #-}
  new = do as <- new
           bs <- new
           return $ MV_2 as bs
  {-# INLINE new #-}
  copy (MV_2 va vb) (MV_2 wa wb) = copy va wa >> copy vb wb
  {-# INLINE copy        #-}
  move (MV_2 va vb) (MV_2 wa wb) = move va wa >> move vb wb
  {-# INLINE move        #-}
  unsafeRead  (MV_2 v w) i = do a <- unsafeRead v i
                                b <- unsafeRead w i
                                return (a,b)
  {-# INLINE unsafeRead  #-}
  unsafeWrite (MV_2 v w) i (a,b) = unsafeWrite v i a >> unsafeWrite w i b
  {-# INLINE unsafeWrite #-}


instance ( Arity n
         , IVector (Vec n) a, IVector (Vec n) b
         ) => IVector (Vec n) (a,b) where
  unsafeFreeze (MV_2 v w)   = do as <- unsafeFreeze v
                                 bs <- unsafeFreeze w
                                 return $ V_2 as bs
  {-# INLINE unsafeFreeze #-}
  unsafeThaw   (V_2  v w)   = do as <- unsafeThaw v
                                 bs <- unsafeThaw w
                                 return $ MV_2 as bs
  {-# INLINE unsafeThaw   #-}
  unsafeIndex  (V_2  v w) i = (unsafeIndex v i, unsafeIndex w i)
  {-# INLINE unsafeIndex  #-}




data instance MVec n s (a,b,c) = MV_3 !(MVec n s a) !(MVec n s b) !(MVec n s c)
data instance Vec  n   (a,b,c) = V_3  !(Vec  n   a) !(Vec  n   b) !(Vec  n   c)

instance (Unbox n a, Unbox n b, Unbox n c) => Unbox n (a,b,c)

instance (Arity n, MVector (MVec n) a, MVector (MVec n) b, MVector (MVec n) c
         ) => MVector (MVec n) (a,b,c) where
  overlaps (MV_3 va vb vc) (MV_3 wa wb wc)
    = overlaps va wa || overlaps vb wb || overlaps vc wc
  {-# INLINE overlaps    #-}
  new = do as <- new
           bs <- new
           cs <- new
           return $ MV_3 as bs cs
  {-# INLINE new #-}
  copy (MV_3 va vb vc) (MV_3 wa wb wc)
    = copy va wa >> copy vb wb >> copy vc wc
  {-# INLINE copy        #-}
  move (MV_3 va vb vc) (MV_3 wa wb wc)
    = move va wa >> move vb wb >> move vc wc
  {-# INLINE move        #-}
  unsafeRead  (MV_3 v w u) i = do a <- unsafeRead v i
                                  b <- unsafeRead w i
                                  c <- unsafeRead u i
                                  return (a,b,c)
  {-# INLINE unsafeRead  #-}
  unsafeWrite (MV_3 v w u) i (a,b,c)
    = unsafeWrite v i a >> unsafeWrite w i b >> unsafeWrite u i c
  {-# INLINE unsafeWrite #-}

instance ( Arity n
         , Vector  (Vec n) a, Vector  (Vec n) b, Vector  (Vec n) c
         , IVector (Vec n) a, IVector (Vec n) b, IVector (Vec n) c
         ) => IVector (Vec n) (a,b,c) where
  unsafeFreeze (MV_3 v w u) = do as <- unsafeFreeze v
                                 bs <- unsafeFreeze w
                                 cs <- unsafeFreeze u
                                 return $ V_3 as bs cs
  {-# INLINE unsafeFreeze #-}
  unsafeThaw   (V_3  v w u) = do as <- unsafeThaw v
                                 bs <- unsafeThaw w
                                 cs <- unsafeThaw u
                                 return $ MV_3 as bs cs
  {-# INLINE unsafeThaw   #-}
  unsafeIndex  (V_3 v w u) i
    = (unsafeIndex v i, unsafeIndex w i, unsafeIndex u i)
  {-# INLINE unsafeIndex  #-}
