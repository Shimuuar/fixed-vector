{-# OPTIONS_GHC -fno-warn-orphans #-}
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
  , VecList
  ) where

import Data.Vector.Fixed.Internal.Arity
import Data.Vector.Fixed.Cont     (VecList,Vector(..),VectorN,Dim,length)
import qualified Data.Vector.Fixed.Cont as C
import Data.Vector.Fixed.Internal

import qualified Prelude as P
import Prelude hiding ( replicate,map,zipWith,maximum,minimum,and,or,all,any
                      , foldl,foldr,foldl1,length,sum
                      , head,tail,mapM,mapM_,sequence,sequence_
                      )

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
