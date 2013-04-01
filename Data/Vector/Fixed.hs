{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
-- |
-- Generic API for vectors with fixed length.
--
-- For encoding of vector size library uses Peano naturals defined in
-- the library. At come point in the future it would make sense to
-- switch to new GHC type level numerals.
module Data.Vector.Fixed (
    -- * Vector type class
    -- ** Vector size
    Dim
  , Z
  , S
    -- ** Synonyms for small numerals
  , C.N1
  , C.N2
  , C.N3
  , C.N4
  , C.N5
  , C.N6
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
  , mk1
  , mk2
  , mk3
  , mk4
  , mk5
    -- ** Generic constructor
  , New
  , vec
  , con
  , (|>)
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
  , (!)
    -- ** Comparison
  , eq
    -- ** Maps
  , map
  , mapM
  , mapM_
  , imap
  , imapM
  , imapM_
  , sequence
  , sequence_
  , sequenceA
  , traverse
    -- * Folding
  , foldl
  , foldr
  , foldl1
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
    -- * Data types
  , VecList(..)
  ) where

import Control.Applicative (Applicative(..))
import qualified Data.Foldable    as F
import qualified Data.Traversable as T

import Data.Vector.Fixed.Internal.Arity
import Data.Vector.Fixed.Cont     (Vector(..),VectorN,Dim,length)
import qualified Data.Vector.Fixed.Cont as C
import Data.Vector.Fixed.Internal

import qualified Prelude as P
import Prelude hiding ( replicate,map,zipWith,maximum,minimum,and,or,all,any
                      , foldl,foldr,foldl1,length,sum
                      , head,tail,mapM,mapM_,sequence,sequence_
                      )


-- $construction
--
-- In addition to functions list above it's possible to use tuples in
-- conjunction with 'convert' function to create vectors. For example:
--
-- v = convert (x,y,z)
--
-- It will work on if type of @v@ is know from elsewhere. Same trick
-- could be used to pattern match on the vector with opaque
-- representation using view patterns
--
-- > function :: Vec N3 Double -> ...
-- > function (convert -> (x,y,z)) = ...

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
instance Arity n => Functor (VecList n) where
  fmap = map
instance Arity n => Applicative (VecList n) where
  pure  = replicate
  (<*>) = zipWith ($)
instance Arity n => F.Foldable (VecList n) where
  foldr = foldr
