{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UnboxedTuples         #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Adaptive array type which picks vector representation from type of
-- element of array. For example arrays of @Double@ are backed by
-- @ByteArray@, arrays of @Bool@ are represented as bit-vector, arrays
-- of tuples are products of arrays. 'Unbox' type class is used to
-- describe representation of an array.
module Data.Vector.Fixed.Unboxed(
    -- * Data type
    Vec(..)
  , Vec1
  , Vec2
  , Vec3
  , Vec4
  , Vec5
    -- * Type classes & derivation
  , Unbox
  , UnboxViaPrim
    -- * Concrete representations
  , BitVec
  , T2(..)
  , T3(..)
  ) where

import Control.Applicative   (Const(..))
import Control.DeepSeq       (NFData(..))
import Data.Bits
import Data.Complex
import Data.Coerce
import Data.Data
import Data.Kind
import Data.Functor.Identity (Identity(..))
import Data.Int              (Int8, Int16, Int32, Int64 )
import Data.Monoid           (Monoid(..),Dual(..),Sum(..),Product(..),All(..),Any(..))
import Data.Semigroup        (Semigroup(..))
import Data.Ord              (Down(..))
import Data.Word             (Word,Word8,Word16,Word32,Word64)
import Data.Primitive.Types  (Prim)
import Foreign.Storable      (Storable)
import GHC.TypeLits
import GHC.Exts              (Proxy#, proxy#)
import Prelude               ( Show(..),Eq(..),Ord(..),Num(..),Applicative(..)
                             , Int,Double,Float,Char,Bool(..),($),id,type (~))

import Data.Vector.Fixed           (Dim,Vector(..),ViaFixed(..))
import Data.Vector.Fixed           qualified as F
import Data.Vector.Fixed.Cont      qualified as C
import Data.Vector.Fixed.Mono      qualified as FM
import Data.Vector.Fixed.Cont      (Peano,Arity,ArityPeano,Fun(..),curryFirst)
import Data.Vector.Fixed.Primitive qualified as P



----------------------------------------------------------------
-- Data type
----------------------------------------------------------------

-- | Adaptive array of dimension @n@ and containing elements of type
--   @a@.
newtype Vec (n :: Nat) a = Vec { getVecRepr :: VecRepr n a (EltRepr a) }

type Vec1 = Vec 1
type Vec2 = Vec 2
type Vec3 = Vec 3
type Vec4 = Vec 4
type Vec5 = Vec 5

-- | Type class which selects internal representation of unboxed vector.
--
--   Crucial design constraint is this type class must be
--   GND-derivable. And this rules out anything mentioning 'Fun',
--   since all it's parameters has @nominal@ role. Thus 'Vector' is
--   not GND-derivable and we have to take somewhat roundabout
--   approach.
class ( Dim    (VecRepr n a) ~ Peano n
      , Vector (VecRepr n a) (EltRepr a)
      ) => Unbox n a where
  -- | Vector data type to use as a representation.
  type VecRepr n a :: Type -> Type
  -- | Element data type to use as a representation.
  type EltRepr   a :: Type
  -- | Convert element to its representation
  toEltRepr   :: Proxy# n -> a -> EltRepr a
  -- | Convert element from its representation
  fromEltRepr :: Proxy# n -> EltRepr a -> a

type instance Dim (Vec n)   = Peano n
type instance Dim (Vec n a) = Peano n

instance (Arity n, Unbox n a) => Vector (Vec n) a where
  inspect (Vec v) f
    = inspect v
      (C.dimapFun (fromEltRepr (proxy# @n)) id f)
  construct
    = C.dimapFun (toEltRepr (proxy# @n)) Vec
      (construct @(VecRepr n a) @(EltRepr a))
  {-# INLINE inspect   #-}
  {-# INLINE construct #-}

instance (Arity n, Unbox n a) => FM.Prod a (Vec n a) where
  construct  = construct
  inspect    = inspect
  {-# INLINE construct  #-}
  {-# INLINE inspect    #-}

instance (Arity n, Unbox n a) => FM.Vector a (Vec n a)



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
-- | @since 2.0.1.0
deriving via ViaFixed (Vec n) a instance (Unbox n a, Prim      a) => Prim      (Vec n a)

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

type instance Dim (VecUnit n)   = Peano n
type instance Dim (VecUnit n a) = Peano n

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

-- | Bit vector represented as 64-bit word. This puts upper limit on
--   length of vector. It's not a big problem. 64-element will strain
--   GHC quite a bit.
data BitVec (n :: Nat) a = BitVec Word64

type instance Dim (BitVec n)   = Peano n
type instance Dim (BitVec n a) = Peano n

instance (n <= 64, Arity n, a ~ Bool) => Vector (BitVec n) a where
  inspect (BitVec w) = inspect (C.generate (testBit w))
  construct = C.accum
    (\(Const (i,w)) -> \case
          True  -> Const (i+1, setBit w i)
          False -> Const (i+1, w))
    (\(Const (_,w)) -> BitVec w)
    (Const (0,0))

instance (n <= 64, Arity n) => Unbox n Bool where
  type VecRepr n Bool = BitVec n
  type EltRepr   Bool = Bool
  toEltRepr   _ = id
  fromEltRepr _ = id
  {-# INLINE toEltRepr   #-}
  {-# INLINE fromEltRepr #-}



----------------------------------------------------------------
-- Primitive wrappers
----------------------------------------------------------------

-- | Wrapper for deriving 'Unbox' for data types which are instances
--   of 'P.Prim' type class:
--
-- > deriving via UnboxViaPrim Word instance (C.Arity n) => Unbox n Word
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

deriving newtype instance (n <= 64, Arity n) => Unbox n All
deriving newtype instance (n <= 64, Arity n) => Unbox n Any


----------------------------------------------------------------
-- Tuples
----------------------------------------------------------------

-- | Representation for vector of 2-tuple as two vectors.
data T2 n a b x = T2 !(Vec n a) !(Vec n b)

type instance Dim (T2 n a b)   = Peano n
type instance Dim (T2 n a b x) = Peano n

instance (Arity n, Unbox n a, Unbox n b) => Vector (T2 n a b) (a,b) where
  inspect (T2 vA vB)
    = inspect (C.zipWith (,) cvA cvB)
    where
      cvA = C.ContVec $ inspect vA
      cvB = C.ContVec $ inspect vB
  construct = pairF T2 construct construct
  {-# INLINE construct #-}
  {-# INLINE inspect   #-}

pairF
  :: ArityPeano n
  => (x -> y -> z)
  -> Fun n a x
  -> Fun n b y
  -> Fun n (a,b) z
{-# INLINE pairF #-}
pairF g funA funB = C.accum
  (\(T_pair fA fB) (a,b) -> T_pair (curryFirst fA a) (curryFirst fB b))
  (\(T_pair (Fun x) (Fun y)) -> g x y)
  (T_pair funA funB)

data T_pair a b x y n = T_pair (Fun n a x) (Fun n b y)


-- | Representation for vector of 2-tuple as two vectors.
data T3 n a b c x = T3 !(Vec n a) !(Vec n b) !(Vec n c)

type instance Dim (T3 n a b c)   = Peano n
type instance Dim (T3 n a b c x) = Peano n

instance (Arity n, Unbox n a, Unbox n b, Unbox n c) => Vector (T3 n a b c) (a,b,c) where
  inspect (T3 vA vB vC)
    = inspect (C.zipWith3 (,,) cvA cvB cvC)
    where
      cvA = C.ContVec $ inspect vA
      cvB = C.ContVec $ inspect vB
      cvC = C.ContVec $ inspect vC
  construct = pair3F T3 construct construct construct
  {-# INLINE construct #-}
  {-# INLINE inspect   #-}

pair3F
  :: ArityPeano n
  => (x -> y -> z -> r)
  -> Fun n a x
  -> Fun n b y
  -> Fun n c z
  -> Fun n (a,b,c) r
{-# INLINE pair3F #-}
pair3F g funA funB funC = C.accum
  (\(T_pair3 fA fB fC) (a,b,c) -> T_pair3 (curryFirst fA a)
                                          (curryFirst fB b)
                                          (curryFirst fC c))
  (\(T_pair3 (Fun x) (Fun y) (Fun z)) -> g x y z)
  (T_pair3 funA funB funC)

data T_pair3 a b c x y z n = T_pair3 (Fun n a x) (Fun n b y) (Fun n c z)



instance (Unbox n a, Unbox n b) => Unbox n (a,b) where
  type VecRepr n (a,b) = T2 n a b
  type EltRepr   (a,b) = (a,b)
  toEltRepr   _ = id
  fromEltRepr _ = id

instance (Unbox n a) => Unbox n (Complex a) where
  -- NOTE: It would be nice to have ability to use single buffer say
  --       for `Complex Double`. But buffers seems to be opaque
  type VecRepr n (Complex a) = T2 n a a
  type EltRepr   (Complex a) = (a,a)
  toEltRepr   _ (r :+ i) = (r,i)
  fromEltRepr _ (r,i)    = r :+ i
  {-# INLINE toEltRepr   #-}
  {-# INLINE fromEltRepr #-}
