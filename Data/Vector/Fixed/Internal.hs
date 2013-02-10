{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- |
-- Type classes for generic vectors. This module exposes type classes
-- and auxiliary functions needed to write generic functions not
-- present in the module "Data.Vector.Fixed".
--
-- Implementation is based on
-- <http://unlines.wordpress.com/2010/11/15/generics-for-small-fixed-size-vectors/>
module Data.Vector.Fixed.Internal (
    -- * Type-level naturals
    Z
  , S
    -- ** Synonyms for small numerals
  , N1
  , N2
  , N3
  , N4
  , N5
  , N6
    -- * N-ary functions
  , Fn
  , Fun(..)
  , Arity(..)
    -- * Vector type class
  , Dim
  , Vector(..)
  , VectorN
  , length
  , Id(..)
  ) where

import Data.Complex
import Prelude hiding (length)


----------------------------------------------------------------
-- Naturals
----------------------------------------------------------------

-- | Type level zero
data Z
-- | Successor of n
data S n

type N1 = S Z
type N2 = S N1
type N3 = S N2
type N4 = S N3
type N5 = S N4
type N6 = S N5

----------------------------------------------------------------
-- N-ary functions
----------------------------------------------------------------

-- | Type family for n-ary functions.
type family   Fn n a b
type instance Fn Z     a b = b
type instance Fn (S n) a b = a -> Fn n a b

-- | Newtype wrapper which is used to make 'Fn' injective.
newtype Fun n a b = Fun (Fn n a b)

newtype T_fmap a b n = T_fmap (Fn n a b)

instance Arity n => Functor (Fun n a) where
  fmap (f :: b -> c) (Fun g0 :: Fun n a b)
     = Fun $ accum
             (\(T_fmap g) a -> T_fmap (g a))
             (\(T_fmap x) -> f x)
             (T_fmap g0 :: T_fmap a b n)
  {-# INLINE fmap #-}


-- | Type class for handling /n/-ary functions.
class Arity n where
  -- | Left fold over /n/ elements exposed as n-ary function.
  accum :: (forall k. t (S k) -> a -> t k) -- ^ Fold function
        -> (t Z -> b)                      -- ^ Extract result of fold
        -> t n                             -- ^ Initial value
        -> Fn n a b                        -- ^ Reduction function

  -- | Monadic left fold.
  accumM :: Monad m
         => (forall k. t (S k) -> a -> m (t k)) -- ^ Fold function
         -> (t Z -> m b)                        -- ^ Extract result of fold
         -> m (t n)                             -- ^ Initial value
         -> Fn n a (m b)                        -- ^ Reduction function

  -- | Apply all parameters to the function.
  apply :: (forall k. t (S k) -> (a, t k)) -- ^ Get value to apply to function
        -> t n                             -- ^ Initial value
        -> Fn n a b                        -- ^ N-ary function
        -> b

  -- | Monadic apply
  applyM :: Monad m
         => (forall k. t (S k) -> m (a, t k)) -- ^ Get value to apply to function
         -> t n                               -- ^ Initial value
         -> Fn n a (m b)                      -- ^ N-ary function
         -> m b
  -- | Arity of function.
  arity :: n -> Int

instance Arity Z where
  accum  _ g t = g t
  accumM _ g t = g =<< t
  apply  _ _ h = h
  applyM _ _ h = h
  arity  _ = 0
  {-# INLINE accum  #-}
  {-# INLINE accumM #-}
  {-# INLINE apply  #-}
  {-# INLINE arity  #-}

instance Arity n => Arity (S n) where
  accum  f g t = \a -> accum  f g (f t a)
  accumM f g t = \a -> accumM f g $ flip f a =<< t
  apply  f t h = case f t of (a,u) -> apply f u (h a)
  applyM f t h = do (a,u) <- f t
                    applyM f u (h a)
  arity  n = 1 + arity (prevN n)
    where
      prevN :: S n -> n
      prevN _ = undefined
  {-# INLINE accum  #-}
  {-# INLINE accumM #-}
  {-# INLINE apply  #-}
  {-# INLINE arity  #-}



----------------------------------------------------------------
-- Type class for vectors
----------------------------------------------------------------

-- | Size of vector expressed as type-level natural.
type family Dim (v :: * -> *)

-- | Type class for vectors with fixed length.
class Arity (Dim v) => Vector v a where
  -- | N-ary function for creation of vectors.
  construct :: Fun (Dim v) a (v a)
  -- | Deconstruction of vector.
  inspect   :: v a -> Fun (Dim v) a b -> b

-- | Vector parametrized by length. In ideal world it should be:
--
-- > forall n. (Arity n, Vector (v n) a, Dim (v n) ~ n) => VectorN v a
--
-- Alas polymorphic constraints aren't allowed in haskell.
class (Vector (v n) a, Dim (v n) ~ n) => VectorN v n a

-- | Length of vector. Function doesn't evaluate its argument.
length :: forall v a. Arity (Dim v) => v a -> Int
{-# INLINE length #-}
length _ = arity (undefined :: Dim v)


-- | Strict identity monad
newtype Id a = Id { runID :: a }

instance Monad Id where
  return     = Id
  Id a >>= f = f a
  {-# INLINE return #-}
  {-# INLINE (>>=)  #-}



----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

type instance Dim Complex = N2

instance RealFloat a => Vector Complex a where
  construct = Fun (:+)
  inspect (x :+ y) (Fun f) = f x y


type instance Dim ((,) a) = N2

instance (b~a) => Vector ((,) b) a where
  construct = Fun (,)
  inspect (a,b) (Fun f) = f a b


type instance Dim ((,,) a b) = N3

instance (b~a, c~a) => Vector ((,,) b c) a where
  construct = Fun (,,)
  inspect (a,b,c) (Fun f) = f a b c


type instance Dim ((,,,) a b c) = N4

instance (b~a, c~a, d~a) => Vector ((,,,) b c d) a where
  construct = Fun (,,,)
  inspect (a,b,c,d) (Fun f) = f a b c d


type instance Dim ((,,,,) a b c d) = N5

instance (b~a, c~a, d~a, e~a) => Vector ((,,,,) b c d e) a where
  construct = Fun (,,,,)
  inspect (a,b,c,d,e) (Fun f) = f a b c d e
