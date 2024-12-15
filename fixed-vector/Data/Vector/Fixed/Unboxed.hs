{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Unboxed vectors with fixed length.
module Data.Vector.Fixed.Unboxed(
    -- * Immutable
    Vec(..)
  , Vec1
  , Vec2
  , Vec3
  , Vec4
  , Vec5
    -- * Type classes
  , Unbox
  ) where

import Control.Applicative   (Const(..))
import Control.Monad
import Control.DeepSeq       (NFData(..))
import Data.Complex
import Data.Coerce
import Data.Data
import Data.Kind
import Data.Proxy
import Data.Functor.Identity (Identity(..))
import Data.Int              (Int8, Int16, Int32, Int64 )
import Data.Monoid           (Monoid(..),Dual(..),Sum(..),Product(..),All(..),Any(..))
import Data.Semigroup        (Semigroup(..))
import Data.Ord              (Down(..))
import Data.Word             (Word,Word8,Word16,Word32,Word64)
import Foreign.Storable      (Storable(..))
import GHC.TypeLits
import GHC.Exts              (Proxy#, proxy#)
import Prelude               ( Show(..),Eq(..),Ord(..),Applicative(..)
                             , Int,Double,Float,Char,Bool(..)
                             , ($),(.),id)

import Data.Vector.Fixed (Dim,Vector(..),ViaFixed(..))
import Data.Vector.Fixed.Mutable (Mutable, MVector(..), IVector(..), DimM, constructVec, inspectVec, Arity, index)
import qualified Data.Vector.Fixed.Cont      as C
import           Data.Vector.Fixed.Cont      (Peano,Arity,ArityPeano,Fun)
import qualified Data.Vector.Fixed.Primitive as P

import qualified Data.Vector.Fixed as F
import qualified Prelude

----------------------------------------------------------------
-- Data type
----------------------------------------------------------------

newtype Vec (n :: Nat) a = Vec { getRepr :: VecRepr n a (EltRepr a) }

type Vec1 = Vec 1
type Vec2 = Vec 2
type Vec3 = Vec 3
type Vec4 = Vec 4
type Vec5 = Vec 5


class ( Dim    (VecRepr n a) ~ Peano n
      , Vector (VecRepr n a) (EltRepr a)
      ) => Unbox n a where
  type VecRepr n a :: Type -> Type
  type EltRepr   a :: Type
  toEltRepr   :: Proxy# n -> a -> EltRepr a
  fromEltRepr :: Proxy# n -> EltRepr a -> a

type instance Dim (Vec n) = Peano n

instance (Arity n, Unbox n a) => Vector (Vec n) a where
  inspect (Vec v) f
    = inspect v
      (C.dimapFun (fromEltRepr (proxy# @n)) id f)
  construct
    = C.dimapFun (toEltRepr (proxy# @n)) Vec
      (construct @(VecRepr n a) @(EltRepr a))
  {-# INLINE inspect   #-}
  {-# INLINE construct #-}



----------------------------------------------------------------
-- Generic instances
----------------------------------------------------------------

deriving via ViaFixed (Vec n) a instance (Unbox n a, Show      a) => Show      (Vec n a)
deriving via ViaFixed (Vec n) a instance (Unbox n a, Eq        a) => Eq        (Vec n a)
deriving via ViaFixed (Vec n) a instance (Unbox n a, Ord       a) => Ord       (Vec n a)
deriving via ViaFixed (Vec n) a instance (Unbox n a, NFData    a) => NFData    (Vec n a)
deriving via ViaFixed (Vec n) a instance (Unbox n a, Semigroup a) => Semigroup (Vec n a)
deriving via ViaFixed (Vec n) a instance (Unbox n a, Monoid    a) => Monoid    (Vec n a)
deriving via ViaFixed (Vec n) a instance (Unbox n a, Storable  a) => Storable  (Vec n a)

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

instance F.Arity n => Unbox n () where
  type VecRepr n () = VecUnit n
  type EltRepr   () = ()
  toEltRepr   _ = id
  fromEltRepr _ = id
  {-# INLINE toEltRepr   #-}
  {-# INLINE fromEltRepr #-}

data VecUnit (n :: Nat) a = VecUnit

type instance Dim (VecUnit n) = Peano n

instance F.Arity n => Vector (VecUnit n) () where
  inspect _ fun
    = C.runContVec fun
    $ C.apply (\Proxy -> ((),Proxy)) Proxy
  construct
    = pure VecUnit
  {-# INLINE inspect   #-}
  {-# INLINE construct #-}



----------------------------------------------------------------
-- Boolean

-- FIXME: Do I want more efficient representation? Word64? 64 is enough for everyone?
instance Arity n => Unbox n Bool where
  type VecRepr n Bool = P.Vec n
  type EltRepr   Bool = Word8
  toEltRepr   _ True  = 1
  toEltRepr   _ False = 0
  {-# INLINE toEltRepr #-}
  fromEltRepr _ = (/= 0)
  {-# INLINE fromEltRepr #-}



----------------------------------------------------------------
-- Primitive wrappers
----------------------------------------------------------------

newtype UnboxViaPrim a = UnboxViaPrim a
  deriving newtype P.Prim

instance (C.Arity n, P.Prim a) => Unbox n (UnboxViaPrim a) where
  type VecRepr n (UnboxViaPrim a) = P.Vec n
  type EltRepr   (UnboxViaPrim a) = a
  toEltRepr   _ = coerce
  fromEltRepr _ = coerce
  
deriving via UnboxViaPrim Int    instance (C.Arity n) => Unbox n Int 
deriving via UnboxViaPrim Int8   instance (C.Arity n) => Unbox n Int8
deriving via UnboxViaPrim Int16  instance (C.Arity n) => Unbox n Int16
deriving via UnboxViaPrim Int32  instance (C.Arity n) => Unbox n Int32
deriving via UnboxViaPrim Int64  instance (C.Arity n) => Unbox n Int64
deriving via UnboxViaPrim Word   instance (C.Arity n) => Unbox n Word 
deriving via UnboxViaPrim Word8  instance (C.Arity n) => Unbox n Word8
deriving via UnboxViaPrim Word16 instance (C.Arity n) => Unbox n Word16
deriving via UnboxViaPrim Word32 instance (C.Arity n) => Unbox n Word32
deriving via UnboxViaPrim Word64 instance (C.Arity n) => Unbox n Word64

deriving via UnboxViaPrim Char   instance (C.Arity n) => Unbox n Char
deriving via UnboxViaPrim Float  instance (C.Arity n) => Unbox n Float
deriving via UnboxViaPrim Double instance (C.Arity n) => Unbox n Double


----------------------------------------------------------------
-- Newtypes
----------------------------------------------------------------

deriving newtype instance (Unbox n a) => Unbox n (Const a b)
deriving newtype instance (Unbox n a) => Unbox n (Identity a)
deriving newtype instance (Unbox n a) => Unbox n (Down a)
deriving newtype instance (Unbox n a) => Unbox n (Dual a)
deriving newtype instance (Unbox n a) => Unbox n (Sum  a)
deriving newtype instance (Unbox n a) => Unbox n (Product a)

deriving newtype instance Arity n => Unbox n All
deriving newtype instance Arity n => Unbox n Any


----------------------------------------------------------------
-- Tuples
----------------------------------------------------------------

data T2 n a b = T2 !(Vec n a) !(Vec n b)

-- type Dim (T2 n a b) =

inspectT2 :: (Unbox n a, Unbox n b) => T2 n a b -> Fun (Peano n) (a,b) r -> r
inspectT2 (T2 vA vB) fun = inspect vB (inspect vA (zipF (,) fun))

constructT2 :: (Unbox n a, Unbox n b) => Fun (Peano n) (a,b) (T2 n a b)
constructT2 = zipWithF T2 construct construct

zipF :: ArityPeano n => (a -> b -> c) -> Fun n c r -> Fun n a (Fun n b r)
zipF = Prelude.undefined -- FIXME: inspect depends critically on

zipWithF
  :: ArityPeano n
  => (x -> y -> z)
  -> Fun n a x
  -> Fun n b y
  -> Fun n (a,b) z
zipWithF = Prelude.undefined


----------------------------------------------------------------
-- Complex



-- newtype instance MVec n s (Complex a) = MV_Complex (MVec n s (a,a))
-- newtype instance Vec  n   (Complex a) = V_Complex  (Vec  n   (a,a))

-- instance (Unbox n a) => Unbox n (Complex a)

-- instance (Arity n, MVector (MVec n) a) => MVector (MVec n) (Complex a) where
--   basicNew = MV_Complex `liftM` basicNew
--   {-# INLINE basicNew #-}
--   basicCopy (MV_Complex v) (MV_Complex w) = basicCopy v w
--   {-# INLINE basicCopy        #-}
--   basicUnsafeRead (MV_Complex v) i = do (a,b) <- basicUnsafeRead v i
--                                         return (a :+ b)
--   {-# INLINE basicUnsafeRead  #-}
--   basicUnsafeWrite (MV_Complex v) i (a :+ b) = basicUnsafeWrite v i (a,b)
--   {-# INLINE basicUnsafeWrite #-}

-- instance (Arity n, IVector (Vec n) a) => IVector (Vec n) (Complex a) where
--   basicUnsafeFreeze (MV_Complex v) = V_Complex `liftM` basicUnsafeFreeze v
--   {-# INLINE basicUnsafeFreeze #-}
--   basicThaw   (V_Complex  v) = MV_Complex `liftM` basicThaw v
--   {-# INLINE basicThaw   #-}
--   unsafeIndex (V_Complex v) i =
--     case unsafeIndex v i of (a,b) -> a :+ b
--   {-# INLINE unsafeIndex  #-}



-- ----------------------------------------------------------------
-- -- Tuples
-- data instance MVec n s (a,b) = MV_2 !(MVec n s a) !(MVec n s b)
-- data instance Vec  n   (a,b) = V_2  !(Vec  n   a) !(Vec  n   b)

-- instance (Unbox n a, Unbox n b) => Unbox n (a,b)

-- instance (Arity n, MVector (MVec n) a, MVector (MVec n) b) => MVector (MVec n) (a,b) where
--   basicNew = do as <- basicNew
--                 bs <- basicNew
--                 return $ MV_2 as bs
--   {-# INLINE basicNew #-}
--   basicCopy (MV_2 va vb) (MV_2 wa wb) = basicCopy va wa >> basicCopy vb wb
--   {-# INLINE basicCopy        #-}
--   basicUnsafeRead  (MV_2 v w) i = do a <- basicUnsafeRead v i
--                                      b <- basicUnsafeRead w i
--                                      return (a,b)
--   {-# INLINE basicUnsafeRead  #-}
--   basicUnsafeWrite (MV_2 v w) i (a,b) = basicUnsafeWrite v i a >> basicUnsafeWrite w i b
--   {-# INLINE basicUnsafeWrite #-}


-- instance ( Arity n
--          , IVector (Vec n) a, IVector (Vec n) b
--          ) => IVector (Vec n) (a,b) where
--   basicUnsafeFreeze (MV_2 v w)   = do as <- basicUnsafeFreeze v
--                                       bs <- basicUnsafeFreeze w
--                                       return $ V_2 as bs
--   {-# INLINE basicUnsafeFreeze #-}
--   basicThaw   (V_2  v w)   = do as <- basicThaw v
--                                 bs <- basicThaw w
--                                 return $ MV_2 as bs
--   {-# INLINE basicThaw   #-}
--   unsafeIndex  (V_2  v w) i = (unsafeIndex v i, unsafeIndex w i)
--   {-# INLINE unsafeIndex  #-}




-- data instance MVec n s (a,b,c) = MV_3 !(MVec n s a) !(MVec n s b) !(MVec n s c)
-- data instance Vec  n   (a,b,c) = V_3  !(Vec  n   a) !(Vec  n   b) !(Vec  n   c)

-- instance (Unbox n a, Unbox n b, Unbox n c) => Unbox n (a,b,c)

-- instance (Arity n, MVector (MVec n) a, MVector (MVec n) b, MVector (MVec n) c
--          ) => MVector (MVec n) (a,b,c) where
--   basicNew = do as <- basicNew
--                 bs <- basicNew
--                 cs <- basicNew
--                 return $ MV_3 as bs cs
--   {-# INLINE basicNew #-}
--   basicCopy (MV_3 va vb vc) (MV_3 wa wb wc)
--     = basicCopy va wa >> basicCopy vb wb >> basicCopy vc wc
--   {-# INLINE basicCopy        #-}
--   basicUnsafeRead  (MV_3 v w u) i = do a <- basicUnsafeRead v i
--                                        b <- basicUnsafeRead w i
--                                        c <- basicUnsafeRead u i
--                                        return (a,b,c)
--   {-# INLINE basicUnsafeRead  #-}
--   basicUnsafeWrite (MV_3 v w u) i (a,b,c)
--     = basicUnsafeWrite v i a >> basicUnsafeWrite w i b >> basicUnsafeWrite u i c
--   {-# INLINE basicUnsafeWrite #-}

-- instance ( Arity n
--          , Vector  (Vec n) a, Vector  (Vec n) b, Vector  (Vec n) c
--          , IVector (Vec n) a, IVector (Vec n) b, IVector (Vec n) c
--          ) => IVector (Vec n) (a,b,c) where
--   basicUnsafeFreeze (MV_3 v w u) = do as <- basicUnsafeFreeze v
--                                       bs <- basicUnsafeFreeze w
--                                       cs <- basicUnsafeFreeze u
--                                       return $ V_3 as bs cs
--   {-# INLINE basicUnsafeFreeze #-}
--   basicThaw   (V_3  v w u) = do as <- basicThaw v
--                                 bs <- basicThaw w
--                                 cs <- basicThaw u
--                                 return $ MV_3 as bs cs
--   {-# INLINE basicThaw   #-}
--   unsafeIndex  (V_3 v w u) i
--     = (unsafeIndex v i, unsafeIndex w i, unsafeIndex u i)
--   {-# INLINE unsafeIndex  #-}
