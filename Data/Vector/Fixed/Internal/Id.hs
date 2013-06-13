-- |
-- Strict identity monad
module Data.Vector.Fixed.Internal.Id (
    Id(..)
  ) where

import Control.Applicative (Applicative(..))


-- | Strict identity monad
newtype Id a = Id { runID :: a }

instance Functor Id where
  fmap f (Id a) = Id (f a)
  {-# INLINE fmap #-}

instance Applicative Id where
  pure          = Id
  Id f <*> Id a = Id (f a)
  {-# INLINE pure  #-}
  {-# INLINE (<*>) #-}

instance Monad Id where
  return     = Id
  Id a >>= f = f a
  {-# INLINE return #-}
  {-# INLINE (>>=)  #-}
