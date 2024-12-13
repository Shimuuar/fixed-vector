{-# LANGUAGE CPP                   #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
-- Generic API for vectors with fixed length.
--
-- For encoding of vector size library uses Peano naturals defined in
-- the library. At come point in the future it would make sense to
-- switch to new GHC type level numerals.
--
-- [@Common pitfalls@]
--
-- Library provide instances for tuples. But there's a catch. Tuples
-- are monomorphic in element type. Let consider 2-tuple @(Int,Int)@.
-- Vector type @v@ is @(,) Int@ and only allowed element type is
-- @Int@.  Because of that we cannot change element type and following
-- code will fail:
--
-- > >>> map (== 1) ((1,2) :: (Int,Int))
-- >
-- > <interactive>:3:1:
-- >     Couldn't match type `Int' with `Bool'
-- >     In the expression: F.map (== 1) ((1, 2) :: (Int, Int))
-- >     In an equation for `it': it = map (== 1) ((1, 2) :: (Int, Int))
--
-- To make it work we need to change vector type as well. Functions
-- from module "Data.Vector.Fixed.Generic" provide this functionality.
--
-- > >>> map (== 1) ((1,2) :: (Int,Int)) :: (Bool,Bool)
-- > (True,False)
module Data.Vector.Fixed (
    -- * Vector type class
    -- ** Vector size
    Dim
    -- ** Type class
  , Vector(..)
  , Arity
  , ArityPeano
  , Fun(..)
  , length
    -- ** Synonyms
  , PeanoNum(..)
  , C.N1, C.N2, C.N3, C.N4, C.N5, C.N6, C.N7, C.N8
    -- * Constructors
    -- $construction
    -- ** Small dimensions
    -- $smallDim
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
    -- ** Pattern for low-dimension vectors
  , pattern V1
  , pattern V2
  , pattern V3
  , pattern V4
    -- ** Continuation-based vectors
  , ContVec
  , empty
  , vector
  , C.cvec
    -- ** Functions
  , replicate
  , replicateM
  , generate
  , generateM
  , unfoldr
  , basis
    -- * Modifying vectors
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
    -- ** Comparison
  , eq
  , ord
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
  , sequenceA
  , traverse
  , distribute
  , collect
    -- * Folding
  , foldl
  , foldr
  , foldl1
  , fold
  , foldMap
  , ifoldl
  , ifoldr
  , foldM
  , ifoldM
    -- ** Special folds
  , sum
  , maximum
  , minimum
  , and
  , or
  , all
  , any
  , find
    -- * Zips
  , zipWith
  , zipWith3
  , zipWithM
  , zipWithM_
  , izipWith
  , izipWith3
  , izipWithM
  , izipWithM_
    -- * Deriving and default implementations
  , ViaFixed(..)
    -- ** Storable
    -- $storable
  , defaultAlignemnt
  , defaultSizeOf
  , defaultPeek
  , defaultPoke
    -- ** NFData
  , defaultRnf
    -- * Conversion
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
    -- * Deprecations
  , StorableViaFixed
  , pattern StorableViaFixed
  ) where

import Control.Applicative (Applicative(..))
import Control.DeepSeq     (NFData(..))
import Data.Coerce
import Data.Data           (Data)
import Data.Monoid         (Monoid(..))
import Data.Semigroup      (Semigroup(..))
import qualified Data.Foldable    as F
import qualified Data.Traversable as T
import Foreign.Storable (Storable(..))
import GHC.TypeLits

import Data.Vector.Fixed.Cont     (Vector(..),Dim,length,ContVec,PeanoNum(..),
                                   vector,empty,Arity,ArityPeano,Fun(..),accum,apply,vector)
import qualified Data.Vector.Fixed.Cont as C
import Data.Vector.Fixed.Internal as I

import Prelude (Show(..),Eq(..),Ord(..),Functor(..),id,(.),($),(<$>),undefined)


-- $construction
--
-- There are several ways to construct fixed vectors except using
-- their constructor if it's available. For small ones it's possible
-- to use functions 'mk1', 'mk2', etc.
--
-- >>> mk3 'a' 'b' 'c' :: (Char,Char,Char)
-- ('a','b','c')
--
-- Alternatively one could use 'mkN'. See its documentation for
-- examples.
--
-- Another option is to create tuple and 'convert' it to desired
-- vector type. For example:
--
-- > v = convert (x,y,z)
--
-- It will work on if type of @v@ is know from elsewhere. Same trick
-- could be used to pattern match on the vector with opaque
-- representation using view patterns
--
-- > function :: Vec N3 Double -> ...
-- > function (convert -> (x,y,z)) = ...
--
-- For small vectors pattern synonyms @V2@, @V3$, @V4@ are provided
-- that use same trick internally.


-- $smallDim
--
-- Constructors for vectors with small dimensions.


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

instance Arity n => T.Traversable (VecList n) where
  sequenceA = sequenceA
  traverse  = traverse

deriving via ViaFixed (VecList n) a instance (Arity n, Show      a) => Show      (VecList n a)
deriving via ViaFixed (VecList n) a instance (Arity n, Eq        a) => Eq        (VecList n a)
deriving via ViaFixed (VecList n) a instance (Arity n, Ord       a) => Ord       (VecList n a)
deriving via ViaFixed (VecList n) a instance (Arity n, NFData    a) => NFData    (VecList n a)
deriving via ViaFixed (VecList n) a instance (Arity n, Semigroup a) => Semigroup (VecList n a)
deriving via ViaFixed (VecList n) a instance (Arity n, Monoid    a) => Monoid    (VecList n a)
deriving via ViaFixed (VecList n) a instance (Arity n, Storable  a) => Storable  (VecList n a)



deriving via ViaFixed (VecPeano n) instance (ArityPeano n) => Functor     (VecPeano n)
deriving via ViaFixed (VecPeano n) instance (ArityPeano n) => Applicative (VecPeano n)
deriving via ViaFixed (VecPeano n) instance (ArityPeano n) => F.Foldable  (VecPeano n)

instance ArityPeano n => T.Traversable (VecPeano n) where
  sequenceA = sequenceA
  traverse  = traverse

deriving via ViaFixed (VecPeano n) a instance (ArityPeano n, Show      a) => Show      (VecPeano n a)
deriving via ViaFixed (VecPeano n) a instance (ArityPeano n, Eq        a) => Eq        (VecPeano n a)
deriving via ViaFixed (VecPeano n) a instance (ArityPeano n, Ord       a) => Ord       (VecPeano n a)
deriving via ViaFixed (VecPeano n) a instance (ArityPeano n, NFData    a) => NFData    (VecPeano n a)
deriving via ViaFixed (VecPeano n) a instance (ArityPeano n, Semigroup a) => Semigroup (VecPeano n a)
deriving via ViaFixed (VecPeano n) a instance (ArityPeano n, Monoid    a) => Monoid    (VecPeano n a)
deriving via ViaFixed (VecPeano n) a instance (ArityPeano n, Storable  a) => Storable  (VecPeano n a)



-- | Single-element tuple.
newtype Only a = Only a
                 deriving (Show,Eq,Ord,Data,Functor,F.Foldable,T.Traversable)

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
--   'Storable', 'NFData'.
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
  foldr    = foldr
  {-# INLINE foldr  #-}
-- GHC<9.2 fails to compile this
#if MIN_VERSION_base(4,16,0)
  length _ = length (undefined :: v ())
  {-# INLINE length #-}
#endif


----------------------------------------------------------------
-- Patterns
----------------------------------------------------------------

pattern V1 :: (Vector v a, Dim v ~ C.N1) => a -> v a
pattern V1 x <- (convert -> (Only x)) where
  V1 x = mk1 x
#if MIN_VERSION_base(4,16,0)
{-# INLINE   V1 #-}
{-# COMPLETE V1 #-}
#endif

pattern V2 :: (Vector v a, Dim v ~ C.N2) => a -> a -> v a
pattern V2 x y <- (convert -> (x,y)) where
  V2 x y = mk2 x y
#if MIN_VERSION_base(4,16,0)
{-# INLINE   V2 #-}
{-# COMPLETE V2 #-}
#endif

pattern V3 :: (Vector v a, Dim v ~ C.N3) => a -> a -> a -> v a
pattern V3 x y z <- (convert -> (x,y,z)) where
  V3 x y z = mk3 x y z
#if MIN_VERSION_base(4,16,0)
{-# INLINE   V3 #-}
{-# COMPLETE V3 #-}
#endif

pattern V4 :: (Vector v a, Dim v ~ C.N4) => a -> a -> a -> a -> v a
pattern V4 t x y z <- (convert -> (t,x,y,z)) where
  V4 t x y z = mk4 t x y z
#if MIN_VERSION_base(4,16,0)
{-# INLINE   V4 #-}
{-# COMPLETE V4 #-}
#endif

----------------------------------------------------------------
-- Deprecation
----------------------------------------------------------------

type StorableViaFixed v a = ViaFixed v a
pattern StorableViaFixed :: v a -> ViaFixed v a
pattern StorableViaFixed v = ViaFixed v
{-# DEPRECATED StorableViaFixed "Use ViaFixed" #-}


-- $setup
--
-- >>> import Data.Char
