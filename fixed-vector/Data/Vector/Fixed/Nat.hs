{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
-- |
module Data.Vector.Fixed.Nat where

import Control.Applicative   ((<|>), Const(..))
import Data.Coerce
import Data.Complex          (Complex(..))
import Data.Data             (Data)
import Data.Kind             (Type)
import Data.Functor.Identity (Identity(..))
import Data.Typeable         (Proxy(..))
import qualified Data.Foldable    as F
import qualified Data.Traversable as F
import Unsafe.Coerce       (unsafeCoerce)
import GHC.TypeLits


data N = Z
       | S Natural

type family Peano (n :: Nat) :: N where
  Peano 0 = Z
  Peano n = S (n - 1)

type family FnPeano (n :: N) (a :: Type) (b :: Type) where
  FnPeano Z     a b = b
  FnPeano (S n) a b = a -> Fn n a b

type family Fn (n :: Nat) (a :: Type) (b :: Type) where
  Fn n a b = FnPeano (Peano n) a b

newtype Fun n a b = Fun { unFun :: Fn n a b }




class Arity (n :: Nat) where
  accum :: (forall k. ( Peano k ~ S n
                      , n ~ k - 1
                      ) => (t k -> a -> t n))
        -> (t 0 -> b)
        -> t n
        -> Fun n a b

instance {-# OVERLAPPING #-} Arity 0 where
  accum _ fini t = Fun $ fini t

instance {-# OVERLAPPABLE #-}
  ( Peano n ~ S k
  , k       ~ n - 1 -- Required for step (GHC can't unify here!)
  -- , k + 1   ~ n     -- Required for constraint in accum 
  , Arity k
  ) => Arity n where
  accum step fini t = Fun $ \a -> undefined -- unFun $ accum step fini (step t a)


----------------------------------------------------------------
-- Fun function and instances
----------------------------------------------------------------

type Add1 n = Peano (n + 1) ~ S n

-- | Prepend ignored parameter to function
constFun
  :: ( Add1 n -- Trivially true!
     )
  => Fun n a b -> Fun (n + 1) a b
constFun (Fun f) = Fun $ \_ -> f
{-# INLINE constFun #-}

-- | Curry first parameter of n-ary function
curryFirst :: (Add1 n) => Fun (n + 1) a b -> a -> Fun n a b
curryFirst = coerce
{-# INLINE curryFirst #-}

-- | Uncurry first ptorcarameter of n-ary function
uncurryFirst :: (Add1 n) => (a -> Fun n a b) -> Fun (n + 1) a b
uncurryFirst = coerce
{-# INLINE uncurryFirst #-}


-- instance Arity n => Functor (Fun n a) where
--   fmap f fun
--      = accum (\(T_Flip g) a -> T_Flip (curryFirst g a))
--              (\(T_Flip x)   -> f (unFun x))
--              (T_Flip fun)
--   {-# INLINE fmap #-}

newtype T_Flip a b n = T_Flip (Fun n a b)

stepFun
  :: ( Peano k ~ S n
     , Add1 n
     )
  => T_Flip a b k -> a -> T_Flip a b n
stepFun (T_Flip (Fun f)) a = T_Flip (Fun (f a))
