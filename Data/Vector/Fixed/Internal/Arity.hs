{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE DeriveDataTypeable    #-}
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
    -- ** Combinators
  , apFun
  , apLast
  , constFun
  , stepFun
  , hideLast
  , shuffleFun
  ) where

import Control.Applicative (Applicative(..))
import Data.Typeable       (Typeable)



----------------------------------------------------------------
-- Naturals
----------------------------------------------------------------

-- | Type level zero
data Z   deriving Typeable
-- | Successor of n
data S n deriving Typeable

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

-- | Newtype wrapper which is used to make 'Fn' injective. It's also a
--   reader monad.
newtype Fun n a b = Fun { unFun :: Fn n a b }


instance Arity n => Functor (Fun n a) where
  fmap (f :: b -> c) (Fun g0 :: Fun n a b)
     = Fun $ accum
             (\(T_fmap g) a -> T_fmap (g a))
             (\(T_fmap x) -> f x)
             (T_fmap g0 :: T_fmap a b n)
  {-# INLINE fmap #-}

instance Arity n => Applicative (Fun n a) where
  pure (x :: x) = Fun $ accum (\(T_pure r) (_::a) -> T_pure r)
                              (\(T_pure r)        -> r)
                              (T_pure x :: T_pure x n)
  (Fun f0 :: Fun n a (p -> q)) <*> (Fun g0 :: Fun n a p)
    = Fun $ accum (\(T_ap f g) a -> T_ap (f a) (g a))
                  (\(T_ap f g)   -> f g)
                  (T_ap f0 g0 :: T_ap a (p -> q) p n)
  {-# INLINE pure  #-}
  {-# INLINE (<*>) #-}

instance Arity n => Monad (Fun n a) where
  return  = pure
  f >>= g = shuffleFun g <*> f
  {-# INLINE return #-}
  {-# INLINE (>>=)  #-}


newtype T_fmap a b   n = T_fmap (Fn n a b)
data    T_pure a     n = T_pure a
data    T_ap   a b c n = T_ap (Fn n a b) (Fn n a c)



----------------------------------------------------------------
-- Generic operations of N-ary functions
----------------------------------------------------------------

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

  -- | Reverse order of parameters.
  reverseF :: Fun n a b -> Fun n a b



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
  reverseF = id
  {-# INLINE accum     #-}
  {-# INLINE accumM    #-}
  {-# INLINE applyFun  #-}
  {-# INLINE applyFunM #-}
  {-# INLINE arity     #-}
  {-# INLINE reverseF  #-}


instance Arity n => Arity (S n) where
  accum     f g t = \a -> accum  f g (f t a)
  accumM    f g t = \a -> accumM f g $ flip f a =<< t
  applyFun  f t h = case f t of (a,u) -> applyFun f u (h a)
  applyFunM f t h = do (a,u) <- f t
                       applyFunM f u (h a)
  arity    _ = 1 + arity (undefined :: n)
  reverseF f = Fun $ \a -> unFun (reverseF $ fmap ($ a) $ hideLast f) 
  {-# INLINE accum     #-}
  {-# INLINE accumM    #-}
  {-# INLINE applyFun  #-}
  {-# INLINE applyFunM #-}
  {-# INLINE arity     #-}
  {-# INLINE reverseF  #-}



----------------------------------------------------------------
-- Combinators
----------------------------------------------------------------

-- | Apply single parameter to function
apFun :: Fun (S n) a b -> a -> Fun n a b
apFun (Fun f) x = Fun (f x)
{-# INLINE apFun #-}

-- | Apply last parameter to function
apLast :: Arity n => Fun (S n) a b -> a -> Fun n a b
apLast f x = fmap ($ x) $ hideLast f
{-# INLINE apLast #-}


-- | Add one parameter to function which is ignored.
constFun :: Fun n a b -> Fun (S n) a b
constFun (Fun f) = Fun $ \_ -> f
{-# INLINE constFun #-}

stepFun :: (Fun n a b -> Fun m a c) -> Fun (S n) a b -> Fun (S m) a c
stepFun g f = Fun $ unFun . g . apFun f
{-# INLINE stepFun #-}

-- | Move last parameter into function result
hideLast :: forall n a b. Arity n => Fun (S n) a b -> Fun n a (a -> b)
hideLast (Fun f0) = Fun $ accum step fini start
  where
    step :: forall k. T_fun a b (S k) -> a -> T_fun a b k
    step = \(T_fun f) a -> T_fun (f a)
    --
    fini :: T_fun a b Z -> (a -> b)
    fini = \(T_fun f) -> f 
    --
    start :: T_fun a b n
    start = T_fun f0
  
newtype T_fun a b n = T_fun (Fn (S n) a b)


-- | Move function parameter to the result of N-ary function.
shuffleFun :: forall n a b r. Arity n
           => (b -> Fun n a r) -> Fun n a (b -> r)
{-# INLINE shuffleFun #-}
shuffleFun f0
  = Fun $ accum (\(T_shuffle f) a -> T_shuffle $ \x -> f x a)
                (\(T_shuffle f)   -> f)
                (T_shuffle (fmap unFun f0) :: T_shuffle b a r n)

newtype T_shuffle x a r n = T_shuffle (x -> Fn n a r)
