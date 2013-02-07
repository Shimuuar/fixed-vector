{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- |
-- Continuations-based API
module Data.Vector.Fixed.Cont (
    ContVec
  , runContVec
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
newtype ContVec r n a = ContVec { runContVec :: Fun n a r -> r }


instance Arity n => Functor (ContVec r n) where
  fmap f (ContVec cont) = ContVec $ \g -> cont (fmapF f g)
  {-# INLINE fmap #-}

data T_vfmap a r n = T_vfmap (Fn n a r)

fmapF :: forall n a b r. Arity n => (a -> b) -> Fun n b r -> Fun n a r
fmapF f (Fun gB) = Fun $
  accum (\(T_vfmap g) b -> T_vfmap (g (f b)))
        (\(T_vfmap r)   -> r)
        (  T_vfmap gB :: T_vfmap b r n)



instance Arity n => Applicative (ContVec r n) where
  pure = ContVec . replicateF
  ContVec contF <*> ContVec contA = ContVec $
    \funB -> contF $ fmap contA $ zipWithF ($) funB
  {-# INLINE pure  #-}
  {-# INLINE (<*>) #-}

-- Implementation of pure
data T_replicate n = T_replicate

replicateF :: forall n a b. Arity n => a -> Fun n a b -> b
replicateF x (Fun h)
  = apply (\T_replicate -> (x, T_replicate))
          (T_replicate :: T_replicate n)
          h

-- Implementation of <*>
data T_zip a c r n = T_zip (VecList n a) (Fn n c r)

zipWithF :: forall n a b c r. Arity n
         => (a -> b -> c) -> Fun n c r -> Fun n a (Fun n b r)
zipWithF f (Fun g0) =
  fmap (\v -> Fun $ accum
              (\(T_zip (VecList (a:as)) g) b -> T_zip (VecList as) (g (f a b)))
              (\(T_zip _ x) -> x)
              (T_zip v g0 :: T_zip a c r n)
       ) construct


----------------------------------------------------------------
-- Functions
----------------------------------------------------------------

cvec :: (Vector v a, Dim v ~ n) => v a -> ContVec r n a
cvec v = ContVec $ inspect v
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
