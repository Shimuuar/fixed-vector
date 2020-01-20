{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
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
  , VectorN
  , Arity
  , Fun(..)
  , length
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
  -- , C.Index
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
    -- * Storable methods
    -- $storable
  , defaultAlignemnt
  , defaultSizeOf
  , defaultPeek
  , defaultPoke
    -- * NFData
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
  ) where

import Control.Applicative (Applicative(..),(<$>))
import Control.DeepSeq     (NFData(..))
import Data.Data           (Typeable,Data)
import Data.Monoid         (Monoid(..))
import Data.Semigroup      (Semigroup(..))
import qualified Data.Foldable    as F
import qualified Data.Traversable as T
import Foreign.Storable (Storable(..))
import Foreign.Ptr      (castPtr)
import GHC.TypeLits

import Data.Vector.Fixed.Cont     (Vector(..),VectorN,Dim,length,ContVec,PeanoNum(..),
                                   vector,empty,Arity,Fun(..),accum,apply,vector)
import qualified Data.Vector.Fixed.Cont as C
import Data.Vector.Fixed.Internal

import Prelude (Show(..),Eq(..),Ord(..),Functor(..),id,(.),($),undefined)
-- Needed for doctest
import Prelude (Char)


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
-- examples
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
  deriving (Typeable)

instance (Arity n, NFData a) => NFData (VecList n a) where
  rnf = defaultRnf
  {-# INLINE rnf #-}

type instance Dim (VecList n) = n

instance Arity n => Vector (VecList n) a where
  construct = fmap VecList $ accum
    (\(T_List f) a -> T_List (f . Cons a))
    (\(T_List f)   -> f Nil)
    (T_List id :: T_List a (C.Peano n) (C.Peano n))
  inspect (VecList v)
    = inspect (apply step (Flip v) :: C.ContVec n a)
    where
      step :: Flip VecPeano a ('S k)  -> (a, Flip VecPeano a k)
      step (Flip (Cons a xs)) = (a, Flip xs)
  {-# INLINE construct #-}
  {-# INLINE inspect   #-}
instance Arity n => VectorN VecList n a

newtype Flip f a n = Flip (f n a)
newtype T_List a n k = T_List (VecPeano k a -> VecPeano n a)


-- Standard instances
instance (Show a, Arity n) => Show (VecList n a) where
  show = show . foldr (:) []
instance (Eq a, Arity n) => Eq (VecList n a) where
  (==) = eq
instance (Ord a, Arity n) => Ord (VecList n a) where
  compare = ord
instance Arity n => Functor (VecList n) where
  fmap = map
instance Arity n => Applicative (VecList n) where
  pure  = replicate
  (<*>) = zipWith ($)
instance Arity n => F.Foldable (VecList n) where
  foldr = foldr
instance Arity n => T.Traversable (VecList n) where
  sequenceA = sequenceA
  traverse  = traverse
instance (Arity n, Monoid a) => Monoid (VecList n a) where
  mempty  = replicate mempty
  mappend = zipWith mappend
  {-# INLINE mempty  #-}
  {-# INLINE mappend #-}

instance (Arity n, Semigroup a) => Semigroup (VecList n a) where
  (<>) = zipWith (<>)
  {-# INLINE (<>) #-}


instance (Storable a, Arity n) => Storable (VecList n a) where
  alignment = defaultAlignemnt
  sizeOf    = defaultSizeOf
  peek      = defaultPeek
  poke      = defaultPoke
  {-# INLINE alignment #-}
  {-# INLINE sizeOf    #-}
  {-# INLINE peek      #-}
  {-# INLINE poke      #-}



-- | Single-element tuple.
newtype Only a = Only a
                 deriving (Show,Eq,Ord,Typeable,Data)

instance Functor Only where
  fmap f (Only a) = Only (f a)
instance F.Foldable Only where
  foldr = foldr
instance T.Traversable Only where
  sequenceA  (Only f) = Only <$> f
  traverse f (Only a) = Only <$> f a
instance Monoid a => Monoid (Only a) where
  mempty = Only mempty
  Only a `mappend` Only b = Only $ mappend a b
instance (Semigroup a) => Semigroup (Only a) where
  Only a <> Only b = Only (a <> b)
  {-# INLINE (<>) #-}


instance NFData a => NFData (Only a) where
  rnf (Only a) = rnf a

type instance Dim Only = 1

instance Vector Only a where
  construct = Fun Only
  inspect (Only a) (Fun f) = f a
  {-# INLINE construct #-}
  {-# INLINE inspect   #-}

instance (Storable a) => Storable (Only a) where
  alignment _ = alignment (undefined :: a)
  sizeOf    _ = sizeOf    (undefined :: a)
  peek p          = Only <$> peek (castPtr p)
  poke p (Only a) = poke (castPtr p) a
  {-# INLINE alignment #-}
  {-# INLINE sizeOf    #-}
  {-# INLINE peek      #-}
  {-# INLINE poke      #-}


-- | Empty tuple.
data Empty a = Empty
  deriving (Show,Eq,Ord)
-- GHC7.10 wants standalone deriving for some reason:
-- >    No instance for (Typeable a)
-- >      arising from the 'deriving' clause of a data type declaration
deriving instance Typeable a => Typeable (Empty a)
deriving instance Data     a => Data     (Empty a)

instance Functor Empty where
  fmap _ Empty = Empty
instance F.Foldable Empty where
  foldr = foldr
instance T.Traversable Empty where
  sequenceA Empty = pure Empty
  traverse _ Empty = pure Empty

instance NFData (Empty a) where
  rnf Empty = ()

type instance Dim Empty = 0

instance Vector Empty a where
  construct = Fun Empty
  inspect _ (Fun b) = b
  {-# INLINE construct #-}
  {-# INLINE inspect   #-}

type Tuple2 a = (a,a)
type Tuple3 a = (a,a,a)
type Tuple4 a = (a,a,a,a)
type Tuple5 a = (a,a,a,a,a)
