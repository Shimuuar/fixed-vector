{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- |
-- Implementation bits os API for fixed length vectors. It's based on
-- ideas described here:
-- <http://unlines.wordpress.com/2010/11/15/generics-for-small-fixed-size-vectors/>
--
-- Generally there is no need to import this module unless you want to
-- implement generic function which could not be expressed using
-- combinators from "Data.Vector.Fixed"
module Data.Vector.Fixed.Internal (
    -- * Type-level naturals
    Z
  , S
    -- * N-ary functions
  , Fn
  , Fun(..)
  , Arity(..)
    -- * Vector type class
  , Dim
  , Vector(..)
  , length
    -- * Fusion
  , Cont(..)
  , create
  , inspectV
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
  accum :: (forall k. tag (S k) -> a -> tag k) -- ^ Fold function
        -> (tag Z -> b)                        -- ^ Extract result of fold
        -> tag n                               -- ^ Initial value
        -> Fn n a b                            -- ^ Reduction function
  -- | Monadic left fold.
  accumM :: Monad m
         => (forall k. tag (S k) -> a -> m (tag k))
         -> (tag Z -> m b)
         -> m (tag n)
         -> Fn n a (m b)
  -- | Apply all parameters to the function. 
  apply :: (forall k. tag (S k) -> (a, tag k)) -- ^ Get value to apply to function
        -> tag n                               -- ^ Initial value
        -> Fn n a b                            -- ^ N-ary function
        -> b
  -- | Monadic apply
  applyM :: Monad m
         => (forall k. tag (S k) -> m (a, tag k)) -- ^ Get value to apply to function
         -> tag n                                 -- ^ Initial value
         -> Fn n a b                              -- ^ N-ary function
         -> m b
  -- | Arity of function.
  arity :: n -> Int

instance Arity Z where
  accum  _ g t = g t
  accumM _ g t = g =<< t
  apply  _ _ h = h
  applyM _ _ h = return h
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

-- | Type family for vector size.
type family Dim (v :: * -> *)

-- | Type class for short vectors with fixed length
class Arity (Dim v) => Vector v a where
  -- | N-ary function for creation of vectors.
  construct :: Fun (Dim v) a (v a)
  -- | Deconstruction of vector.
  inspect   :: v a -> Fun (Dim v) a b -> b

-- | Length of vector. Function doesn't evaluate its argument.
length :: forall v a. Arity (Dim v) => v a -> Int
{-# INLINE length #-}
length _ = arity (undefined :: Dim v)


----------------------------------------------------------------
-- Fusion
----------------------------------------------------------------

newtype Cont n a = Cont (forall r. Fun n a r -> r)

create :: (Arity (Dim v), Vector v a) => Cont (Dim v) a -> v a
{-# INLINE[1] create #-}
create (Cont f) = f construct

inspectV :: (Arity (Dim v), Vector v a) => v a -> Fun (Dim v) a b -> b
{-# INLINE[1] inspectV #-}
inspectV = inspect

app :: Cont n a -> Fun n a b -> b
{-# INLINE app #-}
app (Cont f) g = f g

{-# RULES "inspect/construct"
      forall f g. inspectV (create f) g = app f g
  #-}



----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

type instance Dim Complex = S (S Z)

instance RealFloat a => Vector Complex a where
  construct = Fun (:+)
  inspect (x :+ y) (Fun f) = f x y
