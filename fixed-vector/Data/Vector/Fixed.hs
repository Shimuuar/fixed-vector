{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE DeriveDataTypeable  #-}
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
  , Z
  , S
    -- ** Synonyms for small numerals
  , N1
  , N2
  , N3
  , N4
  , N5
  , N6
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
    -- ** Consing
  , ContVec
  , empty
  , vector
  , (<|)
    -- ** Variadic function
  , Make
  , mkN
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
  , distributeM
  , collectM
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
    -- * Zips
  , zipWith
  , zipWithM
  , izipWith
  , izipWithM
    -- * Conversion
  , convert
  , toList
  , fromList
  , fromList'
  , fromListM
  , fromFoldable
    -- * Data types
  , VecList(..)
  , Only(..)
  , Empty(..)
    -- ** Tuple synonyms
  , Tuple2
  , Tuple3
  , Tuple4
  , Tuple5
  ) where

import Control.Applicative (Applicative(..),(<$>))
import Data.Data           (Typeable,Data)
import Data.Monoid         (Monoid(..))
import qualified Data.Foldable    as F
import qualified Data.Traversable as T

import Data.Vector.Fixed.Cont     (Vector(..),VectorN,Dim,length,ContVec,vector,
                                   empty,S,Z,Arity,Fun(..),accum,apply,
                                   N1,N2,N3,N4,N5,N6,vector)
import qualified Data.Vector.Fixed.Cont as C
import Data.Vector.Fixed.Internal

import Prelude (Show(..),Eq(..),Ord(..),Functor(..),id,(.),($))


-- $construction
--
-- There are several ways to construct fixed vectors except using
-- their constructor if it's available. For small ones it's possible
-- to use functions 'mk1', 'mk2', etc.
-- 
-- >>> mk3 'a' 'b' 'c' :: (Char,Char,Char)
-- ('a','b','c')
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
-- Third way is to use variadic function 'mkN'. It works similarly to
-- 'Text.Printf.printf' except it produces result of type 'ContVec'
-- which should be converted to vector of desired type by 'vector':
--
-- >>> vector $ mkN 'a' 'b' 'c' :: (Char,Char,Char)
-- ('a','b','c')
--
-- Probably most generic way is to cons values to the @ContVec@ and
-- convert it vector of desired type using 'vector':
--
-- >>> vector $ 'a' <| 'b' <| 'c' <| empty :: (Char,Char,Char)
-- ('a','b','c')



-- $smallDim
--
-- Constructors for vectors with small dimensions.



--------------------------------------------------------------------------------
-- We are trying to be clever with indexing here. It's not possible to
-- write generic indexing function. For example it's necessary O(n)
-- for VecList. It's however possible to write O(1) indexing for some
-- vectors and we trying to use such functions where possible.
--
-- We try to use presumable more efficient basicIndex
--
--  1. It should not interfere with deforestation. So we should
--     rewrite only when deforestation rule already fired.
--     (starting from phase 1).
--
--  2. Creation of vector is costlier than generic indexing so we should
--     apply rule only when vector is created anyway
--
-- In order to avoid firing this rule on implementation of (!) it has
-- been necessary to move definition of all functions to internal module.

{-# RULES
"fixed-vector:index/basicIndex"[1] forall vv i.
  runIndex i (C.cvec vv) = C.basicIndex vv i
 #-}


-- | Vector based on the lists. Not very useful by itself but is
--   necessary for implementation.
data VecList n a where
  Nil  :: VecList Z a
  Cons :: a -> VecList n a -> VecList (S n) a
  deriving (Typeable)

-- Vector instance
type instance Dim (VecList n) = n

instance Arity n => Vector (VecList n) a where
  construct = Fun $ accum
    (\(T_List f) a -> T_List (f . Cons a))
    (\(T_List f)   -> f Nil)
    (T_List id :: T_List a n n)
  inspect v (Fun f) = apply step (Flip v) f
    where
      step :: Flip VecList a (S k)  -> (a, Flip VecList a k)
      step (Flip (Cons a xs)) = (a, Flip xs)
  {-# INLINE construct #-}
  {-# INLINE inspect   #-}
instance Arity n => VectorN VecList n a

newtype Flip f a n = Flip (f n a)

newtype T_List a n k = T_List (VecList k a -> VecList n a)


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

type instance Dim Only = S Z

instance Vector Only a where
  construct = Fun Only
  inspect (Only a) (Fun f) = f a
  {-# INLINE construct #-}
  {-# INLINE inspect   #-}

-- | Empty tuple.
data Empty a = Empty deriving (Typeable, Data)

instance Functor Empty where
  fmap _ Empty = Empty
instance F.Foldable Empty where
  foldr = foldr
instance T.Traversable Empty where
  sequenceA Empty = pure Empty
  traverse _ Empty = pure Empty

type instance Dim Empty = Z

instance Vector Empty a where
  construct = Fun Empty
  inspect _ (Fun b) = b
  {-# INLINE construct #-}
  {-# INLINE inspect   #-}

type Tuple2 a = (a,a)
type Tuple3 a = (a,a,a)
type Tuple4 a = (a,a,a,a)
type Tuple5 a = (a,a,a,a,a)
