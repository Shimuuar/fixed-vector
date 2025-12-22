{-# LANGUAGE MagicHash             #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UnboxedTuples         #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- @fixed-vector@ library provides general API for working with short
-- N-element arrays. Functions in this module work on data types which
-- are instances of 'Vector' type class. We provide instances for data
-- types from @base@: tuples, 'Data.Complex.Complex', and few others.
-- There are several length polymorphic arrays:
--
--  * Lazy boxed arrays "Data.Vector.Fixed.Boxed".
--
--  * Strict boxed arrays "Data.Vector.Fixed.Strict".
--
--  * Arrays backed by single @ByteArray@: "Data.Vector.Fixed.Primitive".
--
--  * Arrays backed by pinned memory: "Data.Vector.Fixed.Storable".
--
--  * Arrays which infer array representation from element data type:
--    "Data.Vector.Fixed.Unboxed"
--
--  * Continuation based 'Data.Vector.Fixed.Cont.ContVec' which used
--    by library internally.
--
-- Type level naturals don't have support for induction so all type
-- level computation with length and indices are done using Peano
-- numerals ('PeanoNum'). Type level naturals are only used as type
-- parameters for defining length of arrays.
--
-- [@Instances for tuples@]
--
-- Library provides instances for tuples. They however come with caveat.
-- Let look at 'Vector' instance for 2-tuple:
--
-- > instance b ~ a => Vector ((,) b) a
--
-- Tuple could only be @Vector@ instance if all elements have same
-- type.  so first element fixes type of second one. Thus functions
-- which change element type like 'map' won't work:
--
-- > >>> map (== 1) ((1,2) :: (Int,Int))
-- >
-- > <interactive>:3:1:
-- >     Couldn't match type `Int' with `Bool'
-- >     In the expression: F.map (== 1) ((1, 2) :: (Int, Int))
-- >     In an equation for `it': it = map (== 1) ((1, 2) :: (Int, Int))
--
-- This could be solved either by switching to @ContVec@ manually:
--
-- >>> (vector . map (==1) . cvec) ((1, 2) :: Tuple2 Int) :: Tuple2 Bool
-- (True,False)
--
-- or by using functions genereic in vector type from module
-- "Data.Vector.Fixed.Generic".
module Data.Vector.Fixed (
    -- * Vector type class
    Vector(..)
  , Dim
  , Arity
  , ArityPeano
  , Fun(..)
  , length
    -- ** Peano numbers
  , PeanoNum(..)
  , C.Peano
  , C.N1, C.N2, C.N3, C.N4, C.N5, C.N6, C.N7, C.N8
    -- * Construction and destructions
    -- $construction

    -- ** Constructors
  , mk0
  , mk1
  , mk2
  , mk3
  , mk4
  , mk5
  , mk6
  , mk7
  , mk8
  , mkN
    -- ** Pattern synonyms
  , pattern V1
  , pattern V2
  , pattern V3
  , pattern V4
    -- * Functions
    -- ** Creation
  , replicate
  , replicateM
  , generate
  , generateM
  , unfoldr
  , basis
    -- ** Transformations
  , head
  , tail
  , cons
  , snoc
  , concat
  , reverse
    -- ** Indexing & lenses
  , C.Index
  , (!)
  , index
  , set
  , element
  , elementTy
    -- ** Maps
  , map
  , mapM
  , mapM_
  , imap
  , imapM
  , imapM_
  , scanl
  , scanl1
  , sequence
  , sequence_
  , traverse
  , distribute
  , collect
    -- ** Folds
  , foldl
  , foldl'
  , foldr
  , foldl1
  , fold
  , foldMap
  , ifoldl
  , ifoldr
  , foldM
  , ifoldM
    -- *** Special folds
  , sum
  , maximum
  , minimum
  , and
  , or
  , all
  , any
  , find
    -- ** Zips
  , zipWith
  , zipWith3
  , zipWithM
  , zipWithM_
  , izipWith
  , izipWith3
  , izipWithM
  , izipWithM_
    -- *** Special zips
  , eq
  , ord
    -- ** Conversion
  , convert
  , toList
  , fromList
  , fromList'
  , fromListM
  , fromFoldable
    -- * Data types
  , VecList(..)
  , VecPeano(..)
  , Only(..)
  , Empty(..)
    -- ** Tuple synonyms
  , Tuple2
  , Tuple3
  , Tuple4
  , Tuple5
    -- ** Continuation-based vectors
  , ContVec
  , empty
  , vector
  , cvec
    -- * Instance deriving
  , ViaFixed(..)
    -- ** Storable
    -- $storable
  , defaultAlignemnt
  , defaultSizeOf
  , defaultPeek
  , defaultPoke
    -- ** NFData
  , defaultRnf
    -- * Deprecated functions
  , sequenceA
  ) where

import Control.Applicative     (Applicative(..))
import Control.DeepSeq         (NFData(..))
import Control.Monad.Primitive (PrimBase(..))
import Data.Coerce
import Data.Data               (Data)
import Data.Monoid             (Monoid(..))
import Data.Semigroup          (Semigroup(..))
import Data.Foldable           qualified as F
import Data.Traversable        qualified as T
import Data.Foldable1          qualified as F1
import Data.Primitive.Types    (Prim(..))
import Foreign.Storable        (Storable(..))
import GHC.TypeLits
import GHC.Exts                (Proxy#,proxy#,(*#),(+#),Int(..),Int#)
import GHC.ST                  (ST(..))

import Data.Vector.Fixed.Cont     (Vector(..),Dim,length,ContVec,PeanoNum(..),
                                   vector,cvec,empty,Arity,ArityPeano,Fun(..),accum,apply)
import Data.Vector.Fixed.Cont     qualified as C
import Data.Vector.Fixed.Internal as I

import Prelude (Show(..),Eq(..),Ord(..),Num(..),Functor(..),id,(.),($),(<$>),undefined,flip)


-- $construction
--
-- There are several ways to construct fixed vectors except using
-- their constructor if it's available. For small ones it's possible
-- to use functions 'mk1', 'mk2', etc.
--
-- >>> mk3 'a' 'b' 'c' :: (Char,Char,Char)
-- ('a','b','c')
--
-- Another way is to use pattern synonyms for construction and
-- inspection of vectors:
--
-- >>> V2 'a' 'b' :: (Char,Char)
-- ('a','b')
--
-- >>> case ('a','b') of V2 a b -> [a,b]
-- "ab"
--
-- Last option is to use 'convert' to convert between different vector
-- types of same length. For example
--
-- > v = convert (x,y,z)
--
-- This could be used in view patterns as well:
--
-- > foo :: Vec3 Double -> Foo
-- > foo (convert -> (x,y,z)) = ...
--
-- Pattern synonyms use this trick internally.


-- $storable
--
-- Default implementation of methods for Storable type class assumes
-- that individual elements of vector are stored as N-element array.


-- | Type-based vector with statically known length parametrized by
--   GHC's type naturals
newtype VecList (n :: Nat) a = VecList (VecPeano (C.Peano n) a)

-- | Standard GADT-based vector with statically known length
--   parametrized by Peano numbers.
data VecPeano (n :: PeanoNum) a where
  Nil  :: VecPeano 'Z a
  Cons :: a -> VecPeano n a -> VecPeano ('S n) a

type instance Dim (VecList  n) = C.Peano n
type instance Dim (VecPeano n) = n

instance Arity n => Vector (VecList n) a where
  construct = VecList <$> construct @(VecPeano (C.Peano n)) @a
  inspect (VecList v) = inspect v
  {-# INLINE construct #-}
  {-# INLINE inspect   #-}

instance C.ArityPeano n => Vector (VecPeano n) a where
  construct = accum
    (\(T_List f) a -> T_List (f . Cons a))
    (\(T_List f)   -> f Nil)
    (T_List id :: T_List a n n)
  inspect v
    = inspect (apply step (Flip v) :: C.ContVec n a)
    where
      step :: Flip VecPeano a ('S k)  -> (a, Flip VecPeano a k)
      step (Flip (Cons a xs)) = (a, Flip xs)
  {-# INLINE construct #-}
  {-# INLINE inspect   #-}

newtype Flip f a n = Flip (f n a)
newtype T_List a n k = T_List (VecPeano k a -> VecPeano n a)



deriving via ViaFixed (VecList n) instance (Arity n) => Functor     (VecList n)
deriving via ViaFixed (VecList n) instance (Arity n) => Applicative (VecList n)
deriving via ViaFixed (VecList n) instance (Arity n) => F.Foldable  (VecList n)
-- | @since @2.0.1.0
deriving via ViaFixed (VecList n)
    instance (Arity n, C.Peano n ~ S k) => F1.Foldable1 (VecList n)

instance Arity n => T.Traversable (VecList n) where
  sequence  = sequence
  sequenceA = sequence
  traverse  = mapM
  mapM      = mapM
  {-# INLINE sequence  #-}
  {-# INLINE sequenceA #-}
  {-# INLINE mapM      #-}
  {-# INLINE traverse  #-}

deriving via ViaFixed (VecList n) a instance (Arity n, Show      a) => Show      (VecList n a)
deriving via ViaFixed (VecList n) a instance (Arity n, Eq        a) => Eq        (VecList n a)
deriving via ViaFixed (VecList n) a instance (Arity n, Ord       a) => Ord       (VecList n a)
deriving via ViaFixed (VecList n) a instance (Arity n, NFData    a) => NFData    (VecList n a)
deriving via ViaFixed (VecList n) a instance (Arity n, Semigroup a) => Semigroup (VecList n a)
deriving via ViaFixed (VecList n) a instance (Arity n, Monoid    a) => Monoid    (VecList n a)
deriving via ViaFixed (VecList n) a instance (Arity n, Storable  a) => Storable  (VecList n a)
-- | @since 2.0.1.0
deriving via ViaFixed (VecList n) a instance (Arity n, Prim      a) => Prim      (VecList n a)



deriving via ViaFixed (VecPeano n) instance (ArityPeano n) => Functor     (VecPeano n)
deriving via ViaFixed (VecPeano n) instance (ArityPeano n) => Applicative (VecPeano n)
deriving via ViaFixed (VecPeano n) instance (ArityPeano n) => F.Foldable  (VecPeano n)
-- | @since @2.0.1.0
deriving via ViaFixed (VecPeano n)
    instance (ArityPeano n, n ~ S k) => F1.Foldable1 (VecPeano n)

instance ArityPeano n => T.Traversable (VecPeano n) where
  sequence  = sequence
  sequenceA = sequence
  traverse  = mapM
  mapM      = mapM
  {-# INLINE sequence  #-}
  {-# INLINE sequenceA #-}
  {-# INLINE mapM      #-}
  {-# INLINE traverse  #-}

deriving via ViaFixed (VecPeano n) a instance (ArityPeano n, Show      a) => Show      (VecPeano n a)
deriving via ViaFixed (VecPeano n) a instance (ArityPeano n, Eq        a) => Eq        (VecPeano n a)
deriving via ViaFixed (VecPeano n) a instance (ArityPeano n, Ord       a) => Ord       (VecPeano n a)
deriving via ViaFixed (VecPeano n) a instance (ArityPeano n, NFData    a) => NFData    (VecPeano n a)
deriving via ViaFixed (VecPeano n) a instance (ArityPeano n, Semigroup a) => Semigroup (VecPeano n a)
deriving via ViaFixed (VecPeano n) a instance (ArityPeano n, Monoid    a) => Monoid    (VecPeano n a)
deriving via ViaFixed (VecPeano n) a instance (ArityPeano n, Storable  a) => Storable  (VecPeano n a)
-- | @since 2.0.1.0
deriving via ViaFixed (VecPeano n) a instance (ArityPeano n, Prim      a) => Prim      (VecPeano n a)



-- | Single-element tuple.
newtype Only a = Only a
                 deriving (Show,Eq,Ord,Data,Functor,F.Foldable,T.Traversable)

-- | @since @2.0.1.0
deriving via ViaFixed Only instance F1.Foldable1 Only


instance Monoid a => Monoid (Only a) where
  mempty  = Only mempty
  mappend = (<>)
instance (Semigroup a) => Semigroup (Only a) where
  (<>) = coerce ((<>) @a)
  {-# INLINE (<>) #-}


instance NFData a => NFData (Only a) where
  rnf (Only a) = rnf a

type instance Dim Only = C.N1

instance Vector Only a where
  construct = Fun Only
  inspect (Only a) (Fun f) = f a
  {-# INLINE construct #-}
  {-# INLINE inspect   #-}

instance (Storable a) => Storable (Only a) where
  alignment = coerce (alignment @a)
  sizeOf    = coerce (sizeOf    @a)
  peek      = coerce (peek      @a)
  poke      = coerce (poke      @a)


-- | Empty tuple.
data Empty a = Empty
  deriving (Show,Eq,Ord,Data,Functor,F.Foldable,T.Traversable)

instance NFData (Empty a) where
  rnf Empty = ()

type instance Dim Empty = 'Z

instance Vector Empty a where
  construct = Fun Empty
  inspect _ (Fun b) = b
  {-# INLINE construct #-}
  {-# INLINE inspect   #-}

type Tuple2 a = (a,a)
type Tuple3 a = (a,a,a)
type Tuple4 a = (a,a,a,a)
type Tuple5 a = (a,a,a,a,a)


----------------------------------------------------------------
-- Deriving
----------------------------------------------------------------

-- | Newtype for deriving instance for data types which has instance
--   of 'Vector'. It supports 'Eq', 'Ord', 'Semigroup', 'Monoid',
--   'Storable', 'NFData', 'Functor', 'Applicative', 'Foldable'.
newtype ViaFixed v a = ViaFixed (v a)

type instance Dim (ViaFixed v) = Dim v

instance Vector v a => Vector (ViaFixed v) a where
  construct = ViaFixed <$> construct
  inspect (ViaFixed v) = inspect v
  {-# INLINE construct #-}
  {-# INLINE inspect   #-}

instance (Vector v a, Show a) => Show (ViaFixed v a) where
  showsPrec = coerce (I.showsPrec @v @a)

instance (Vector v a, Eq a) => Eq (ViaFixed v a) where
  (==) = coerce (eq @v @a)
  {-# INLINE (==) #-}

instance (Vector v a, Ord a) => Ord (ViaFixed v a) where
  compare = coerce (ord @v @a)
  {-# INLINE compare #-}

instance (Vector v a, NFData a) => NFData (ViaFixed v a) where
  rnf = coerce (defaultRnf @a @v)
  {-# INLINE rnf #-}

instance (Vector v a, Semigroup a) => Semigroup (ViaFixed v a) where
  (<>) = coerce (zipWith @v @a (<>))
  {-# INLINE (<>) #-}

instance (Vector v a, Monoid a) => Monoid (ViaFixed v a) where
  mempty = coerce (replicate @v @a mempty)
  {-# INLINE mempty #-}

instance (Vector v a, Storable a) => Storable (ViaFixed v a) where
  alignment = coerce (defaultAlignemnt @a @v)
  sizeOf    = coerce (defaultSizeOf    @a @v)
  peek      = coerce (defaultPeek      @a @v)
  poke      = coerce (defaultPoke      @a @v)
  {-# INLINE alignment #-}
  {-# INLINE sizeOf    #-}
  {-# INLINE peek      #-}
  {-# INLINE poke      #-}

-- | @since 2.0.1.0
instance (Vector v a, Prim a) => Prim (ViaFixed v a) where
  sizeOf# _ = sizeOf# (undefined :: a) *# dim where
    dim = case C.peanoToInt (proxy# @(Dim v)) of I# i -> i
  alignment# _ = alignment# (undefined :: a)
  {-# INLINE sizeOf#    #-}
  {-# INLINE alignment# #-}
  -- Bytearray
  indexByteArray# ba k
    = generate $ \(I# i) -> indexByteArray# ba (off +# i)
    where
      off = vectorOff (proxy# @(Dim v))  k
  readByteArray# ba k
    = internal
    $ generateM
    $ \(I# i) -> ST (\s -> readByteArray# ba (off +# i) s)
    where
      off = vectorOff (proxy# @(Dim v))  k
  writeByteArray# ba k (ViaFixed vec) =
    case loop of
      ST st -> \s -> case st s of
                       (# s', () #) -> s'
    where
      off  = vectorOff (proxy# @(Dim v))  k
      loop = flip imapM_ vec $ \(I# i) a -> ST $ \s ->
        (# writeByteArray# ba (off +# i) a s, () #)
  {-# INLINE indexByteArray# #-}
  {-# INLINE readByteArray#  #-}
  {-# INLINE writeByteArray# #-}
  -- Addr
  indexOffAddr# addr k
    = generate $ \(I# i) -> indexOffAddr# addr (off +# i)
    where
      off = vectorOff (proxy# @(Dim v))  k
  readOffAddr# ba k
    = internal
    $ generateM
    $ \(I# i) -> ST (\s -> readOffAddr# ba (off +# i) s)
    where
      off = vectorOff (proxy# @(Dim v))  k
  writeOffAddr# addr k (ViaFixed vec) =
    case loop of
      ST st -> \s -> case st s of
                       (# s', () #) -> s'
    where
      off  = vectorOff (proxy# @(Dim v))  k
      loop = flip imapM_ vec $ \(I# i) a -> ST $ \s ->
        (# writeOffAddr# addr (off +# i) a s, () #)
  {-# INLINE indexOffAddr# #-}
  {-# INLINE readOffAddr#  #-}
  {-# INLINE writeOffAddr# #-}


vectorOff :: (ArityPeano n) => Proxy# n -> Int# -> Int#
{-# INLINE vectorOff #-}
vectorOff n k =
  case C.peanoToInt n of
    I# dim -> dim *# k


instance (forall a. Vector v a) => Functor (ViaFixed v) where
  fmap = map
  {-# INLINE fmap #-}

instance (forall a. Vector v a) => Applicative (ViaFixed v) where
  pure   = replicate
  (<*>)  = zipWith ($)
  liftA2 = zipWith
  a <* _ = a
  _ *> b = b
  {-# INLINE pure   #-}
  {-# INLINE (<*>)  #-}
  {-# INLINE (<*)   #-}
  {-# INLINE (*>)   #-}
  {-# INLINE liftA2 #-}

instance (forall a. Vector v a) => F.Foldable (ViaFixed v) where
  foldMap' f = foldl' (\ acc a -> acc <> f a) mempty
  foldr      = foldr
  foldl      = foldl
  foldl'     = foldl'
  toList     = toList
  sum        = sum
  product    = foldl' (*) 0
  length     = length
  {-# INLINE foldMap' #-}
  {-# INLINE foldr    #-}
  {-# INLINE foldl    #-}
  {-# INLINE foldl'   #-}
  {-# INLINE toList   #-}
  {-# INLINE sum      #-}
  {-# INLINE product  #-}
  {-# INLINE length   #-}


-- | @since @2.0.1.0
instance (forall a. Vector v a, Dim v ~ S k) => F1.Foldable1 (ViaFixed v) where
  fold1       = foldl1 (<>)
  foldMap1  f = F1.foldMap1  f . cvec
  foldMap1' f = F1.foldMap1' f . cvec
  toNonEmpty  = F1.toNonEmpty . cvec
  head        = head
  last        = F1.last . cvec
  maximum     = maximum
  minimum     = minimum
  {-# INLINE fold1      #-}
  {-# INLINE foldMap1   #-}
  {-# INLINE foldMap1'  #-}
  {-# INLINE toNonEmpty #-}
  {-# INLINE maximum    #-}
  {-# INLINE minimum    #-}
  {-# INLINE head       #-}
  {-# INLINE last       #-}


----------------------------------------------------------------
-- Patterns
----------------------------------------------------------------

pattern V1 :: (Vector v a, Dim v ~ C.N1) => a -> v a
pattern V1 x <- (convert -> (Only x)) where
  V1 x = mk1 x
{-# INLINE   V1 #-}
{-# COMPLETE V1 #-}

pattern V2 :: (Vector v a, Dim v ~ C.N2) => a -> a -> v a
pattern V2 x y <- (convert -> (x,y)) where
  V2 x y = mk2 x y
{-# INLINE   V2 #-}
{-# COMPLETE V2 #-}

pattern V3 :: (Vector v a, Dim v ~ C.N3) => a -> a -> a -> v a
pattern V3 x y z <- (convert -> (x,y,z)) where
  V3 x y z = mk3 x y z
{-# INLINE   V3 #-}
{-# COMPLETE V3 #-}

pattern V4 :: (Vector v a, Dim v ~ C.N4) => a -> a -> a -> a -> v a
pattern V4 t x y z <- (convert -> (t,x,y,z)) where
  V4 t x y z = mk4 t x y z
{-# INLINE   V4 #-}
{-# COMPLETE V4 #-}

-- $setup
--
-- >>> import Data.Char
-- >>> import Prelude (Int,Bool(..))
