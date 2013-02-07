{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- |
-- Continuations-based API
module Data.Vector.Fixed.Cont (
    -- * Vector as continuation
    ContVecT
  , runContVecT
  -- ** Non-monadic vector
  , ContVec
  , runContVec
  -- ** Inserting vector
  , cvec
    -- * Running ContVec
  , vector
    -- * Data types
  , VecList(..) -- FIXME: unsafe
  ) where

import Control.Applicative
import Data.Vector.Fixed.Internal


----------------------------------------------------------------
-- Cont. vectors and their instances
----------------------------------------------------------------

-- | Vector as continuation.
newtype ContVecT r m n a = ContVecT { runContVecT :: Fun n a (m r) -> m r }

type ContVec r = ContVecT r Id

runContVec :: Arity n => ContVec r n a -> Fun n a r -> r
runContVec (ContVecT c) f = runID $ c (fmap return f)
{-# INLINE runContVec #-}

instance (Arity n, Monad m) => Functor (ContVecT r m n) where
  fmap f (ContVecT cont) = ContVecT $ \g -> cont (fmapF f g)
  {-# INLINE fmap #-}

data T_vfmap a r n = T_vfmap (Fn n a r)

fmapF :: forall n a b r. Arity n => (a -> b) -> Fun n b r -> Fun n a r
fmapF f (Fun gB) = Fun $
  accum (\(T_vfmap g) b -> T_vfmap (g (f b)))
        (\(T_vfmap r)   -> r)
        (  T_vfmap gB :: T_vfmap b r n)



instance (Arity n, Monad m) => Applicative (ContVecT r m n) where
  pure = ContVecT . replicateF
  ContVecT contF <*> ContVecT contA = ContVecT $
    \funB -> contF $ fmap contA $ izipWithFM (\_ f a -> return (f a)) funB
  {-# INLINE pure  #-}
  {-# INLINE (<*>) #-}

-- Implementation of pure
data T_replicate n = T_replicate

replicateF :: forall n a b. Arity n => a -> Fun n a b -> b
replicateF x (Fun h)
  = apply (\T_replicate -> (x, T_replicate))
          (T_replicate :: T_replicate n)
          h

data T_izip a c r n = T_izip Int (VecList n a) (Fn n c r)

-- FIXME: explain function
izipWithFM :: forall m n a b c r. (Arity n, Monad m)
           => (Int -> a -> b -> m c) -> Fun n c (m r) -> Fun n a (Fun n b (m r))
{-# INLINE izipWithFM #-}
izipWithFM f (Fun g0) =
  fmap (\v -> Fun $ accumM
              (\(T_izip i (VecList (a:as)) g) b -> do x <- f i a b
                                                      return $ T_izip (i+1) (VecList as) (g x)
              )
              (\(T_izip _ _ x) -> x)
              (return $ T_izip 0 v g0 :: m (T_izip a c (m r) n))
       ) construct


----------------------------------------------------------------
-- Functions
----------------------------------------------------------------

cvec :: (Vector v a, Dim v ~ n, Monad m) => v a -> ContVecT r m n a
cvec v = ContVecT $ inspect v
{-# INLINE cvec #-}

vector :: (Vector v a, Dim v ~ n) => ContVec (v a) n a -> v a
vector c = runContVec c construct
{-# INLINE vector #-}


-- | Vector based on the lists. Not very useful by itself but is
--   necessary for implementation.
newtype VecList n a = VecList [a]
                      deriving (Show,Eq)

type instance Dim (VecList n) = n

newtype Flip f a n = Flip (f n a)

newtype T_list a n = T_list ([a] -> [a])

-- It's vital to avoid 'reverse' and build list using [a]->[a]
-- functions. Reverse is recursive and interferes with inlining.
instance Arity n => Vector (VecList n) a where
  construct = Fun $ accum
    (\(T_list xs) x -> T_list (xs . (x:)))
    (\(T_list xs) -> VecList (xs []) :: VecList n a)
    (T_list id :: T_list a n)
  inspect v (Fun f) = apply
    (\(Flip (VecList (x:xs))) -> (x, Flip (VecList xs)))
    (Flip v)
    f
  {-# INLINE construct #-}
  {-# INLINE inspect   #-}
instance Arity n => VectorN VecList n a
