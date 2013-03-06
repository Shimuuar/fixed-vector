{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- |
-- Type class for working with N-ary functions
module Data.Vector.Fixed.Internal.Arity (
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
  ) where

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
