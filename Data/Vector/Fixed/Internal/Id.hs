-- |
-- Strict identity monad
module Data.Vector.Fixed.Internal.Id (
    Id(..)
  ) where

-- | Strict identity monad
newtype Id a = Id { runID :: a }

instance Monad Id where
  return     = Id
  Id a >>= f = f a
  {-# INLINE return #-}
  {-# INLINE (>>=)  #-}
