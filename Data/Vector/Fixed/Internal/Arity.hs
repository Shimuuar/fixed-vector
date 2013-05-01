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
  , apply
  , applyM
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
newtype Fun n a b = Fun { unFun :: Fn n a b }

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
  applyFun :: (forall k. t (S k) -> (a, t k)) -- ^ Get value to apply to function
           -> t n                             -- ^ Initial value
           -> Fn n a b                        -- ^ N-ary function
           -> (b, t Z)

  -- | Monadic apply
  applyFunM :: Monad m
            => (forall k. t (S k) -> m (a, t k)) -- ^ Get value to apply to function
            -> t n                               -- ^ Initial value
            -> Fn n a (m b)                      -- ^ N-ary function
            -> m (b, t Z)
  -- | Arity of function.
  arity :: n -> Int

-- | Apply all parameters to the function.
apply :: Arity n
      => (forall k. t (S k) -> (a, t k)) -- ^ Get value to apply to function
      -> t n                             -- ^ Initial value
      -> Fn n a b                        -- ^ N-ary function
      -> b
{-# INLINE apply #-}
apply step z f = fst $ applyFun step z f

-- | Apply all parameters to the function.
applyM :: (Arity n, Monad m)
       => (forall k. t (S k) -> m (a, t k)) -- ^ Get value to apply to function
       -> t n                               -- ^ Initial value
       -> Fn n a (m b)                      -- ^ N-ary function
       -> m b
{-# INLINE applyM #-}
applyM step z f = do
  (r,_) <- applyFunM step z f
  return r

instance Arity Z where
  accum     _ g t = g t
  accumM    _ g t = g =<< t
  applyFun  _ t h = (h,t)
  applyFunM _ t h = do r <- h
                       return (r,t)
  arity  _ = 0
  {-# INLINE accum     #-}
  {-# INLINE accumM    #-}
  {-# INLINE applyFun  #-}
  {-# INLINE applyFunM #-}
  {-# INLINE arity     #-}


instance Arity n => Arity (S n) where
  accum     f g t = \a -> accum  f g (f t a)
  accumM    f g t = \a -> accumM f g $ flip f a =<< t
  applyFun  f t h = case f t of (a,u) -> applyFun f u (h a)
  applyFunM f t h = do (a,u) <- f t
                       applyFunM f u (h a)
  arity _ = 1 + arity (undefined :: n)
  {-# INLINE accum     #-}
  {-# INLINE accumM    #-}
  {-# INLINE applyFun  #-}
  {-# INLINE applyFunM #-}
  {-# INLINE arity     #-}
