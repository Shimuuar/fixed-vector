{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- |
-- Continuations-based API
module Data.Vector.Fixed.Cont (
    -- * Vector as continuation
    ContVecT
  , ContVec
    -- * Construction of ContVec
  , cvec
  , replicate
  , replicateM
  , generate
  , generateM
    -- * Transformations
  , map
  , imap
  , mapM
  , imapM
  , tail
    -- ** Zips
  , zipWith
  , izipWith
  , zipWithM
  , izipWithM
    -- * Running ContVec
  , runContVecT
  , runContVecM
  , runContVec
    -- ** Vector construction
  , vector
  , vectorM
    -- ** Folds
  , foldl
  , foldl1
  , ifoldl
  , foldM
  , ifoldM
    -- * Data types
  , VecList(..) -- FIXME: unsafe
  ) where

import Control.Applicative
import Data.Vector.Fixed.Internal
import Prelude hiding ( replicate,map,zipWith,maximum,minimum
                      , foldl,foldl1,length,sum
                      , head,tail,mapM,mapM_,sequence,sequence_
                      )


----------------------------------------------------------------
-- Cont. vectors and their instances
----------------------------------------------------------------

-- | Vector as continuation.
newtype ContVecT r m n a = ContVecT (Fun n a (m r) -> m r)

-- | Vector as continuation without monadic context
type ContVec r = ContVecT r Id

instance (Arity n) => Functor (ContVecT r m n) where
  fmap = map
  {-# INLINE fmap #-}

instance (Arity n) => Applicative (ContVecT r m n) where
  pure  = replicate
  (<*>) = zipWith ($)
  {-# INLINE pure  #-}
  {-# INLINE (<*>) #-}



----------------------------------------------------------------
-- Construction
----------------------------------------------------------------

-- | Convert regular vector to continuation
cvec :: (Vector v a, Dim v ~ n, Monad m) => v a -> ContVecT r m n a
cvec = ContVecT . inspect
{-# INLINE[1] cvec #-}


-- | Execute monadic action for every element of vector.
replicate :: forall r m n a. (Arity n)
          => a -> ContVecT r m n a
{-# INLINE replicate #-}
replicate a = ContVecT $ \(Fun fun) ->
  apply (\T_replicate -> (a, T_replicate))
        (T_replicate :: T_replicate n)
        fun

-- | Execute monadic action for every element of vector.
replicateM :: forall r m n a. (Arity n, Monad m)
           => m a -> ContVecT r m n a
{-# INLINE replicateM #-}
replicateM act = ContVecT $ \(Fun fun) ->
  applyM (\T_replicate -> do { a <- act; return (a, T_replicate) } )
         (T_replicate :: T_replicate n)
         fun

data T_replicate n = T_replicate


generate :: forall r m n a. (Arity n) => (Int -> a) -> ContVecT r m n a
{-# INLINE generate #-}
generate f = ContVecT $ \(Fun fun) ->
  apply (\(T_generate n) -> (f n, T_generate (n + 1)))
        (T_generate 0 :: T_generate n)
        (fun :: Fn n a (m r))

generateM :: forall m n a r. (Monad m, Arity n)
           => (Int -> m a) -> ContVecT r m n a
{-# INLINE generateM #-}
generateM f = ContVecT $ \(Fun fun) ->
  applyM (\(T_generate n) -> do { a <- f n; return (a, T_generate (n + 1)) } )
         (T_generate 0 :: T_generate n)
         (fun :: Fn n a (m r))

newtype T_generate n = T_generate Int


----------------------------------------------------------------
-- Transforming vectors
----------------------------------------------------------------

map :: forall r m n a b. (Arity n)
     => (a -> b) -> ContVecT r m n a -> ContVecT r m n b
{-# INLINE map #-}
map = imap . const

imap :: forall r m n a b. (Arity n)
     => (Int -> a -> b) -> ContVecT r m n a -> ContVecT r m n b
{-# INLINE imap #-}
imap f (ContVecT contA) = ContVecT $
  contA . imapF f

mapM :: forall r m n a b. (Arity n, Monad m)
     => (a -> m b) -> ContVecT r m n a -> ContVecT r m n b
{-# INLINE mapM #-}
mapM = imapM . const

imapM :: forall r m n a b. (Arity n, Monad m)
     => (Int -> a -> m b) -> ContVecT r m n a -> ContVecT r m n b
{-# INLINE imapM #-}
imapM f (ContVecT contA) = ContVecT $
  contA . imapFM f


imapF :: forall n a b r. Arity n
      => (Int -> a -> b) -> Fun n b r -> Fun n a r
{-# INLINE imapF #-}
imapF f (Fun funB) = Fun $
  accum (\(T_map i g) b -> T_map (i+1) (g (f i b)))
        (\(T_map _ r)   -> r)
        (  T_map 0 funB :: T_map b r n)

imapFM :: forall m n a b r. (Arity n, Monad m)
       => (Int -> a -> m b) -> Fun n b (m r) -> Fun n a (m r)
{-# INLINE imapFM #-}
imapFM f (Fun h) = Fun $
  accumM (\(T_map i g) a -> do b <- f i a
                               return $ T_map (i + 1) (g b))
         (\(T_map _ g)   -> g)
         (return $ T_map 0 h :: m (T_map b (m r) n))

data T_map a r n = T_map Int (Fn n a r)


-- | Get tail
tail :: ContVecT r m (S n) a
     -> ContVecT r m n a
tail (ContVecT cont) = ContVecT $ \(Fun f) -> cont (Fun $ \_ -> f)
{-# INLINE tail #-}

-- | Zip two vector together using function.
zipWith :: forall a b c m r n. (Arity n)
        => (a -> b -> c)
        -> ContVecT r m n a -> ContVecT r m n b -> ContVecT r m n c
{-# INLINE zipWith #-}
zipWith = izipWith . const

-- | Zip two vector together using function which takes element index
--   as well.
izipWith :: forall a b c m r n. (Arity n)
         => (Int -> a -> b -> c)
         -> ContVecT r m n a -> ContVecT r m n b -> ContVecT r m n c
{-# INLINE izipWith #-}
izipWith f (ContVecT contA) (ContVecT contB) = ContVecT $ \funC ->
  contA $ fmap contB $ izipWithF f funC

-- | Zip two vector together using monadic function.
zipWithM :: forall a b c m r n. (Arity n, Monad m)
         => (a -> b -> m c)
         -> ContVecT r m n a -> ContVecT r m n b -> ContVecT r m n c
zipWithM = izipWithM . const

-- | Zip two vector together using monadic function which takes element
--   index as well..
izipWithM :: forall a b c m r n. (Arity n, Monad m)
          => (Int -> a -> b -> m c)
          -> ContVecT r m n a -> ContVecT r m n b -> ContVecT r m n c
{-# INLINE izipWithM #-}
izipWithM f (ContVecT contA) (ContVecT contB) = ContVecT $ \funC ->
  contA $ fmap contB $ izipWithFM f funC



-- FIXME: explain function
izipWithF :: forall n a b c r. (Arity n)
          => (Int -> a -> b -> c) -> Fun n c r -> Fun n a (Fun n b r)
{-# INLINE izipWithF #-}
izipWithF f (Fun g0) =
  fmap (\v -> Fun $ accum
              (\(T_izip i (VecList (a:as)) g) b -> T_izip (i+1) (VecList as) (g $ f i a b)
              )
              (\(T_izip _ _ x) -> x)
              (T_izip 0 v g0 :: (T_izip a c r n))
       ) construct

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

data T_izip a c r n = T_izip Int (VecList n a) (Fn n c r)



----------------------------------------------------------------
-- Folds
----------------------------------------------------------------

runContVecT :: (Monad m, Arity n)
            => Fun n a r
            -> ContVecT r m n a
            -> m r
runContVecT f (ContVecT c) = c $ fmap return f
{-# INLINE runContVecT #-}

runContVecM :: Arity n
            => Fun n a (m r)
            -> ContVecT r m n a
            -> m r
runContVecM f (ContVecT c) = c f
{-# INLINE runContVecM #-}

runContVec :: Arity n
           => Fun n a r
           -> ContVec r n a
           -> r
runContVec f (ContVecT c) = runID $ c (fmap return f)
{-# INLINE runContVec #-}

-- | Convert continuation to the vector.
vector :: (Vector v a, Dim v ~ n) => ContVec (v a) n a -> v a
vector = runContVec construct
{-# INLINE[1] vector #-}

-- | Convert continuation to the vector.
vectorM :: (Vector v a, Dim v ~ n, Monad m) => ContVecT (v a) m n a -> m (v a)
vectorM = runContVecT construct
{-# INLINE[1] vectorM #-}


foldl :: forall n a b. Arity n
      => (b -> a -> b) -> b -> Fun n a b
{-# INLINE foldl #-}
foldl f = ifoldl (\b _ a -> f b a)

ifoldl :: forall n a b. Arity n
       => (b -> Int -> a -> b) -> b -> Fun n a b
{-# INLINE ifoldl #-}
ifoldl f b = Fun $ accum (\(T_ifoldl i r) a -> T_ifoldl (i+1) (f r i a))
                         (\(T_ifoldl _ r) -> r)
                         (T_ifoldl 0 b :: T_ifoldl b n)

foldM :: forall n m a b. (Arity n, Monad m)
      => (b -> a -> m b) -> b -> Fun n a (m b)
{-# INLINE foldM #-}
foldM f x
  = foldl (\m a -> do{ b <- m; f b a}) (return x)

ifoldM :: forall n m a b. (Arity n, Monad m)
      => (b -> Int -> a -> m b) -> b -> Fun n a (m b)
{-# INLINE ifoldM #-}
ifoldM f x
  = ifoldl (\m i a -> do{ b <- m; f b i a}) (return x)

data T_ifoldl b n = T_ifoldl !Int b

-- Implementation of foldl1F is particularly ugly. It could be
-- expressed in terms of foldlF:
--
-- > foldl1F f = Fun $ \a -> case foldlF f a :: Fun n a a of Fun g -> g
--
-- But it require constraint `Arity n` whereas foldl1 provide
-- Arity (S n). Latter imply former but GHC cannot infer it. So
-- 'Arity n' begin to propagate through contexts. It's not acceptable.

newtype T_foldl1 a n = T_foldl1 (Maybe a)

foldl1 :: forall n a. (Arity (S n))
       => (a -> a -> a) -> Fun (S n) a a
{-# INLINE foldl1 #-}
foldl1 f = Fun $ accum (\(T_foldl1 r) a -> T_foldl1 $ Just $ maybe a (flip f a) r)
                       (\(T_foldl1 (Just x)) -> x)
                       (T_foldl1 Nothing :: T_foldl1 a (S n))





----------------------------------------------------------------
-- VecList
----------------------------------------------------------------

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
