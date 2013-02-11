{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Rank2Types            #-}
-- |
-- Continuations-based API
module Data.Vector.Fixed.Cont (
    -- * Vector as continuation
    ContVecT
  , ContVec
    -- ** Synonyms for small numerals
  , N1
  , N2
  , N3
  , N4
  , N5
  , N6
    -- * Construction of ContVec
  , cvec
  , fromList
  , replicate
  , replicateM
  , generate
  , generateM
  , unfoldr
  , basis
    -- ** Constructors
  , mk1
  , mk2
  , mk3
  , mk4
  , mk5
    -- * Transformations
  , map
  , imap
  , mapM
  , imapM
  , tail
  , cons
    -- ** Zips
  , zipWith
  , izipWith
  , zipWithM
  , izipWithM
    -- * Running ContVec
    -- $running
  , runContVecT
  , runContVecM
  , runContVec
    -- ** Getters
  , head
    -- ** Vector construction
  , vector
  , vectorM
    -- ** Folds
  , foldl
  , foldl1
  , foldr
  , ifoldl
  , ifoldr
  , foldM
  , ifoldM
    -- *** Special folds
  , sum
  , minimum
  , maximum
  , and
  , or
  , all
  , any
    -- * Data types
  , VecList
  ) where

import Control.Applicative
import Data.Vector.Fixed.Internal
import Prelude hiding ( replicate,map,zipWith,maximum,minimum,and,or,any,all
                      , foldl,foldr,foldl1,length,sum
                      , head,tail,mapM,mapM_,sequence,sequence_
                      )

----------------------------------------------------------------
-- Cont. vectors and their instances
----------------------------------------------------------------

-- | Vector represented as continuation.
newtype ContVecT m n a = ContVecT (forall r. Fun n a (m r) -> m r)

-- | Vector as continuation without monadic context.
type ContVec = ContVecT Id

instance (Arity n) => Functor (ContVecT m n) where
  fmap = map
  {-# INLINE fmap #-}

instance (Arity n) => Applicative (ContVecT m n) where
  pure  = replicate
  (<*>) = zipWith ($)
  {-# INLINE pure  #-}
  {-# INLINE (<*>) #-}



----------------------------------------------------------------
-- Construction
----------------------------------------------------------------

-- | Convert regular vector to continuation
cvec :: (Vector v a, Dim v ~ n, Monad m) => v a -> ContVecT m n a
cvec v = ContVecT (inspect v)
{-# INLINE[1] cvec #-}

-- | Convert list to continuation-based vector. Will throw error if
--   list is shorter than resulting vector.
fromList :: forall m n a. Arity n => [a] -> ContVecT m n a
{-# INLINE fromList #-}
fromList xs = ContVecT $ \(Fun fun) ->
  apply step
        (T_flist xs :: T_flist a n)
        fun
  where
    step (T_flist []    ) = error "Data.Vector.Fixed.Cont.fromList: too few elements"
    step (T_flist (a:as)) = (a, T_flist as)

data T_flist a n = T_flist [a]


-- | Execute monadic action for every element of vector. Synonym for 'pure'.
replicate :: forall m n a. (Arity n)
          => a -> ContVecT m n a
{-# INLINE replicate #-}
replicate a = ContVecT $ \(Fun fun) ->
  apply (\T_replicate -> (a, T_replicate))
        (T_replicate :: T_replicate n)
        fun

-- | Execute monadic action for every element of vector.
replicateM :: forall m n a. (Arity n, Monad m)
           => m a -> ContVecT m n a
{-# INLINE replicateM #-}
replicateM act = ContVecT $ \(Fun fun) ->
  applyM (\T_replicate -> do { a <- act; return (a, T_replicate) } )
         (T_replicate :: T_replicate n)
         fun

data T_replicate n = T_replicate


-- | Generate vector from function which maps element's index to its value.
generate :: forall m n a. (Arity n) => (Int -> a) -> ContVecT m n a
{-# INLINE generate #-}
generate f = ContVecT $ \(Fun fun) ->
  apply (\(T_generate n) -> (f n, T_generate (n + 1)))
        (T_generate 0 :: T_generate n)
         fun

-- | Generate vector from monadic function which maps element's index
--   to its value.
generateM :: forall m n a. (Monad m, Arity n)
           => (Int -> m a) -> ContVecT m n a
{-# INLINE generateM #-}
generateM f = ContVecT $ \(Fun fun) ->
  applyM (\(T_generate n) -> do { a <- f n; return (a, T_generate (n + 1)) } )
         (T_generate 0 :: T_generate n)
          fun

newtype T_generate n = T_generate Int

-- | Unfold vector.
unfoldr :: forall m n b a. Arity n => (b -> (a,b)) -> b -> ContVecT m n a
{-# INLINE unfoldr #-}
unfoldr f b0 = ContVecT $ \(Fun fun) ->
  apply (\(T_unfoldr b) -> let (a,b') = f b in (a, T_unfoldr b'))
        (T_unfoldr b0 :: T_unfoldr b n)
         fun

newtype T_unfoldr b n = T_unfoldr b


-- | Unit vector along Nth axis.
basis :: forall m n a. (Num a, Arity n) => Int -> ContVecT m n a
{-# INLINE basis #-}
basis n0 = ContVecT $ \(Fun fun) ->
  apply (\(T_basis n) -> ((if n == 0 then 1 else 0) :: a, T_basis (n - 1)))
        (T_basis n0 :: T_basis n)
        fun

newtype T_basis n = T_basis Int


mk1 :: a -> ContVecT m N1 a
mk1 a1 = ContVecT $ \(Fun f) -> f a1
{-# INLINE mk1 #-}

mk2 :: a -> a -> ContVecT m N2 a
mk2 a1 a2 = ContVecT $ \(Fun f) -> f a1 a2
{-# INLINE mk2 #-}

mk3 :: a -> a -> a -> ContVecT m N3 a
mk3 a1 a2 a3 = ContVecT $ \(Fun f) -> f a1 a2 a3
{-# INLINE mk3 #-}

mk4 :: a -> a -> a -> a -> ContVecT m N4 a
mk4 a1 a2 a3 a4 = ContVecT $ \(Fun f) -> f a1 a2 a3 a4
{-# INLINE mk4 #-}

mk5 :: a -> a -> a -> a -> a -> ContVecT m N5 a
mk5 a1 a2 a3 a4 a5 = ContVecT $ \(Fun f) -> f a1 a2 a3 a4 a5
{-# INLINE mk5 #-}


----------------------------------------------------------------
-- Transforming vectors
----------------------------------------------------------------

-- | Map over vector. Synonym for 'fmap'
map :: (Arity n) => (a -> b) -> ContVecT m n a -> ContVecT m n b
{-# INLINE map #-}
map = imap . const

-- | Apply function to every element of the vector and its index.
imap :: (Arity n) => (Int -> a -> b) -> ContVecT m n a -> ContVecT m n b
{-# INLINE imap #-}
imap f (ContVecT contA) = ContVecT $
  contA . imapF f

-- | Monadic map over vector.
mapM :: (Arity n, Monad m) => (a -> m b) -> ContVecT m n a -> ContVecT m n b
{-# INLINE mapM #-}
mapM = imapM . const

-- | Apply monadic function to every element of the vector and its index.
imapM :: (Arity n, Monad m) => (Int -> a -> m b) -> ContVecT m n a -> ContVecT m n b
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


-- | /O(1)/ Tail of vector.
tail :: ContVecT m (S n) a
     -> ContVecT m n a
tail (ContVecT cont) = ContVecT $ \(Fun f) -> cont (Fun $ \_ -> f)
{-# INLINE tail #-}

-- | /O(1)/ Prepend element to vector
cons :: a -> ContVecT m n a -> ContVecT m (S n) a
cons a (ContVecT cont) = ContVecT $ \(Fun f) -> cont $ Fun $ f a
{-# INLINE cons #-}

-- | Zip two vector together using function.
zipWith :: (Arity n) => (a -> b -> c)
        -> ContVecT m n a -> ContVecT m n b -> ContVecT m n c
{-# INLINE zipWith #-}
zipWith = izipWith . const

-- | Zip two vector together using function which takes element index
--   as well.
izipWith :: (Arity n) => (Int -> a -> b -> c)
         -> ContVecT m n a -> ContVecT m n b -> ContVecT m n c
{-# INLINE izipWith #-}
izipWith f (ContVecT contA) (ContVecT contB) = ContVecT $ \funC ->
  contA $ fmap contB $ izipWithF f funC

-- | Zip two vector together using monadic function.
zipWithM :: (Arity n, Monad m) => (a -> b -> m c)
         -> ContVecT m n a -> ContVecT m n b -> ContVecT m n c
{-# INLINE zipWithM #-}
zipWithM = izipWithM . const

-- | Zip two vector together using monadic function which takes element
--   index as well..
izipWithM :: (Arity n, Monad m) => (Int -> a -> b -> m c)
          -> ContVecT m n a -> ContVecT m n b -> ContVecT m n c
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
-- Running vector
----------------------------------------------------------------

-- $running
--
-- Only way to get result from continuation vector is to apply
-- finalizer function to them using 'runContVecT', 'runContVecM' or
-- 'runContVec'. Getters and folds are defined as such finalizer
-- functions.


-- | Run continuation vector using non-monadic finalizer.
runContVecT :: (Monad m, Arity n)
            => Fun n a r        -- ^ finalizer function
            -> ContVecT m n a   -- ^ vector
            -> m r
runContVecT f (ContVecT c) = c $ fmap return f
{-# INLINE runContVecT #-}

-- | Run continuation vector using monadic finalizer.
runContVecM :: Arity n
            => Fun n a (m r)    -- ^ finalizer function
            -> ContVecT m n a   -- ^ vector
            -> m r
runContVecM f (ContVecT c) = c f
{-# INLINE runContVecM #-}

-- | Run continuation vector.
runContVec :: Arity n
           => Fun n a r
           -> ContVec n a
           -> r
runContVec f (ContVecT c) = runID $ c (fmap return f)
{-# INLINE runContVec #-}

-- | Convert continuation to the vector.
vector :: (Vector v a, Dim v ~ n) => ContVec n a -> v a
vector = runContVec construct
{-# INLINE[1] vector #-}

-- | Convert continuation to the vector.
vectorM :: (Vector v a, Dim v ~ n, Monad m) => ContVecT m n a -> m (v a)
vectorM = runContVecT construct
{-# INLINE[1] vectorM #-}

{-# RULES "cvec/vector"
   forall x. cvec (vector x) = x
  #-}


-- | Finalizer function for getting head of the vector.
head :: forall n a. Arity (S n) => Fun (S n) a a
{-# INLINE head #-}
head = Fun $ accum (\(T_head m) a -> T_head $ case m of { Nothing -> Just a; x -> x })
                   (\(T_head (Just x)) -> x)
                   (T_head Nothing :: T_head a (S n))

data T_head a n = T_head (Maybe a)


-- | Left fold over continuation vector.
foldl :: forall n a b. Arity n
      => (b -> a -> b) -> b -> Fun n a b
{-# INLINE foldl #-}
foldl f = ifoldl (\b _ a -> f b a)

-- | Left fold over continuation vector.
ifoldl :: forall n a b. Arity n
       => (b -> Int -> a -> b) -> b -> Fun n a b
{-# INLINE ifoldl #-}
ifoldl f b = Fun $ accum (\(T_ifoldl i r) a -> T_ifoldl (i+1) (f r i a))
                         (\(T_ifoldl _ r) -> r)
                         (T_ifoldl 0 b :: T_ifoldl b n)

-- | Monadic left fold over continuation vector.
foldM :: forall n m a b. (Arity n, Monad m)
      => (b -> a -> m b) -> b -> Fun n a (m b)
{-# INLINE foldM #-}
foldM f x
  = foldl (\m a -> do{ b <- m; f b a}) (return x)

-- | Monadic left fold over continuation vector.
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

-- | Left fold.
foldl1 :: forall n a. (Arity (S n))
       => (a -> a -> a) -> Fun (S n) a a
{-# INLINE foldl1 #-}
foldl1 f = Fun $ accum (\(T_foldl1 r) a -> T_foldl1 $ Just $ maybe a (flip f a) r)
                       (\(T_foldl1 (Just x)) -> x)
                       (T_foldl1 Nothing :: T_foldl1 a (S n))

-- | Right fold over continuation vector
foldr :: forall n a b. Arity n
      => (a -> b -> b) -> b -> Fun n a b
{-# INLINE foldr #-}
foldr = ifoldr . const

-- | Right fold over continuation vector
ifoldr :: forall n a b. Arity n
      => (Int -> a -> b -> b) -> b -> Fun n a b
{-# INLINE ifoldr #-}
ifoldr f z = Fun $
  accum (\(T_ifoldr i g) a -> T_ifoldr (i+1) (g . f i a))
        (\(T_ifoldr _ g)   -> g z)
        (T_ifoldr 0 id :: T_ifoldr b n)


data T_ifoldr b n = T_ifoldr Int (b -> b)

-- | Sum all elements in the vector.
sum :: (Num a, Arity n) => Fun n a a
sum = foldl (+) 0
{-# INLINE sum #-}

-- | Minimal element of vector.
minimum :: (Ord a, Arity (S n)) => Fun (S n) a a
minimum = foldl1 min
{-# INLINE minimum #-}

-- | Maximal element of vector.
maximum :: (Ord a, Arity (S n)) => Fun (S n) a a
maximum = foldl1 max
{-# INLINE maximum #-}

-- | Conjunction of elements of a vector.
and :: Arity n => Fun n Bool Bool
and = foldr (&&) True
{-# INLINE and #-}

-- | Disjunction of all elements of a vector.
or :: Arity n => Fun n Bool Bool
or = foldr (||) False
{-# INLINE or #-}

-- | Determines whether all elements of vector satisfy predicate.
all :: Arity n => (a -> Bool) -> Fun n a Bool
all f = foldr (\x b -> f x && b) True
{-# INLINE all #-}

-- | Determines whether any of element of vector satisfy predicate.
any :: Arity n => (a -> Bool) -> Fun n a Bool
any f = foldr (\x b -> f x && b) True
{-# INLINE any #-}


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
