{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Rank2Types            #-}
-- |
-- Continuations-based API
module Data.Vector.Fixed.Cont (
    S
  , Z
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
    -- * Vector type class
  , Dim
  , Vector(..)
  , VectorN
  , length
  , Index(..)
    -- * Vector as continuation
  , ContVecT(..)
  , ContVec
    -- * Construction of ContVec
  , cvec
  , empty
  , fromList
  , fromList'
  , fromListM
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
  , mkN
    -- * Transformations
  , map
  , imap
  , mapM
  , imapM
  , tail
  , cons
  , consV
  , snoc
  , reverse
  , changeMonad
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
  , index
  , element
  , elementTy
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
  ) where

import Control.Applicative (Applicative(..),(<$>))
import Data.Complex        (Complex(..))
import Data.Typeable       (Typeable(..))
import qualified Data.Foldable    as F
import qualified Data.Traversable as F

import Data.Vector.Fixed.Internal.Id
import Prelude hiding ( replicate,map,zipWith,maximum,minimum,and,or,any,all
                      , foldl,foldr,foldl1,length,sum,reverse
                      , head,tail,mapM,mapM_,sequence,sequence_
                      )

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
  applyMon :: Monad m
           => (forall k. t (S k) -> m (a, t k))
           -> t n
           -> m (Fn n a b -> b)
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
  applyMon  _ _ = return id
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
  applyMon  f t = do (a,t') <- f t
                     cont   <- applyMon f t'
                     return $ \f -> cont (f a)
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

-- | Apply last parameter to function. Unlike 'apFun' we need to
--   traverse all parameters but last hence 'Arity' constraint.
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



----------------------------------------------------------------
-- Type class for fixed vectors
----------------------------------------------------------------

-- | Size of vector expressed as type-level natural.
type family Dim (v :: * -> *)

-- | Type class for vectors with fixed length. Instance should provide
-- two functions: one to create vector and another for vector
-- deconstruction. They must obey following law:
--
-- > inspect v construct = v
class Arity (Dim v) => Vector v a where
  -- | N-ary function for creation of vectors.
  construct :: Fun (Dim v) a (v a)
  -- | Deconstruction of vector.
  inspect   :: v a -> Fun (Dim v) a b -> b
  -- | Optional more efficient implementation of indexing. Shouldn't
  --   be used directly, use 'Data.Vector.Fixed.!' instead.
  basicIndex :: v a -> Int -> a
  basicIndex v i = runContVec (index i) (cvec v)
  {-# INLINE basicIndex #-}

-- | Vector parametrized by length. In ideal world it should be:
--
-- > forall n. (Arity n, Vector (v n) a, Dim (v n) ~ n) => VectorN v a
--
-- Alas polymorphic constraints aren't allowed in haskell.
class (Vector (v n) a, Dim (v n) ~ n) => VectorN v n a

-- | Length of vector. Function doesn't evaluate its argument.
length :: forall v a. Arity (Dim v) => v a -> Int
{-# INLINE length #-}
length _ = arity (undefined :: Dim v)

-- | Type class for indexing of vector when index value is known at
--   compile time.
class Index k n where
  getF  :: k -> Fun n a a
  lensF :: Functor f => k -> (a -> f a) -> Fun n a r -> Fun n a (f r)

instance Arity n => Index Z (S n) where
  getF  _       = Fun $ \(a :: a) -> unFun (pure a :: Fun n a a)
  lensF _ f fun = Fun $ \(a :: a) -> unFun $
    (\g -> g <$> f a) <$> shuffleFun (apFun fun)
  {-# INLINE getF  #-}
  {-# INLINE lensF #-}

instance Index k n => Index (S k) (S n) where
  getF  _       = Fun $ \(_::a) -> unFun (getF  (undefined :: k) :: Fun n a a)
  lensF _ f fun = Fun $ \a      -> unFun (lensF (undefined :: k) f (apFun fun a))
  {-# INLINE getF  #-}
  {-# INLINE lensF #-}



----------------------------------------------------------------
-- Cont. vectors and their instances
----------------------------------------------------------------

-- | Vector represented as continuation. Alternative wording: it's
--   Church encoding of N-ary vector.
newtype ContVecT m n a = ContVecT (forall r. Fun n a (m r) -> m r)

-- | Vector as continuation without monadic context.
type ContVec = ContVecT Id


type instance Dim (ContVecT m n) = n

instance Arity n => Vector (ContVecT Id n) a where
  construct = mkN
  inspect   = flip runContVec
  {-# INLINE construct #-}
  {-# INLINE inspect   #-}

instance Arity n => VectorN (ContVecT Id) n a

instance (Arity n) => Functor (ContVecT m n) where
  fmap = map
  {-# INLINE fmap #-}

instance (Arity n) => Applicative (ContVecT m n) where
  pure  = replicate
  (<*>) = zipWith ($)
  {-# INLINE pure  #-}
  {-# INLINE (<*>) #-}

instance (Arity n) => F.Foldable (ContVecT Id n) where
  foldr f z = runContVec (foldr f z)
  {-# INLINE foldr #-}

instance (Arity n) => F.Traversable (ContVecT Id n) where
  sequenceA v = inspect v $ sequenceAF construct
  {-# INLINE sequenceA #-}

sequenceAF :: forall f n a b. (Applicative f, Arity n)
     => Fun n a b -> Fun n (f a) (f b)
{-# INLINE sequenceAF #-}
sequenceAF (Fun f0)
  = Fun $ accum (\(T_sequenceA f) a -> T_sequenceA (f <*> a))
                (\(T_sequenceA f)   -> f)
                (T_sequenceA (pure f0) :: T_sequenceA f a b n)

newtype T_sequenceA f a b n = T_sequenceA (f (Fn n a b))



-- | Change monad type for the continuation vector.
changeMonad :: (Monad p, Arity n)
            => (forall x. p x -> x) -- ^ Function to extract result from monad
            -> ContVecT p n a -> ContVecT m n a
{-# INLINE changeMonad #-}
changeMonad run (ContVecT cont)
  = ContVecT $ convertCont run return cont

convertCont :: (Arity n)
            => (b -> c)
            -> (c -> b)
            -> (Fun n a b -> b)
            -> (Fun n a c -> c)
{-# INLINE convertCont #-}
convertCont fB2C fC2B cont = \funC ->
  fB2C $ cont (fmap fC2B funC)



----------------------------------------------------------------
-- Construction
----------------------------------------------------------------

-- | Convert regular vector to continuation
cvec :: (Vector v a, Dim v ~ n, Monad m) => v a -> ContVecT m n a
cvec v = ContVecT (inspect v)
{-# INLINE[0] cvec #-}

-- | Create empty vector.
empty :: ContVecT m Z a
{-# INLINE empty #-}
empty = ContVecT (\(Fun r) -> r)

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

-- | Same as 'fromList' bu throws error is list doesn't have same
--   length as vector.
fromList' :: forall m n a. Arity n => [a] -> ContVecT m n a
{-# INLINE fromList' #-}
fromList' xs = ContVecT $ \(Fun fun) ->
  let (r,rest) = applyFun step (T_flist xs :: T_flist a n) fun
      step (T_flist []    ) = error "Data.Vector.Fixed.Cont.fromList': too few elements"
      step (T_flist (a:as)) = (a, T_flist as)
  in case rest of
       T_flist [] -> r
       _          -> error "Data.Vector.Fixed.Cont.fromList': too many elements"

-- | Convert list to continuation-based vector. Will fail with
--   'Nothing' if list doesn't have right length.
fromListM :: forall n a. Arity n => [a] -> ContVecT Maybe n a
{-# INLINE fromListM #-}
fromListM xs = ContVecT $ \(Fun fun) -> do
  (r,rest) <- applyFunM step (T_flist xs :: T_flist a n) fun
  case rest of
    T_flist [] -> return r
    _          -> Nothing
  where
    step (T_flist []    ) = Nothing
    step (T_flist (a:as)) = return (a, T_flist as)


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

-- | N-ary constructor for vectors. It's more generic variant of
--   'contruct' because it works with arbitrary monad type not only
--   'Id'.
mkN :: forall m n a. Arity n => Fun n a (ContVecT m n a)
mkN = Fun $ accum (\(T_mkN f) a -> T_mkN (f . cons a))
                  (\(T_mkN f)   -> f empty)
                  (T_mkN id :: T_mkN n m a n)
{-# INLINE mkN #-}

newtype T_mkN n_tot m a n = T_mkN (ContVecT m n a -> ContVecT m n_tot a)


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
tail (ContVecT cont) = ContVecT $ \f -> cont $ constFun f
{-# INLINE tail #-}

-- | /O(1)/ Prepend element to vector
cons :: a -> ContVecT m n a -> ContVecT m (S n) a
cons a (ContVecT cont) = ContVecT $ \f -> cont $ apFun f a
{-# INLINE cons #-}

-- | Prepend single element to vector.
consV :: forall m n a. Monad m => ContVecT m (S Z) a -> ContVecT m n a -> ContVecT m (S n) a
{-# INLINE consV #-}
consV (ContVecT cont1) (ContVecT cont)
  = ContVecT $ \f -> do a <- cont1 $ Fun return
                        cont $ apFun f a 


-- | /O(1)/ Append element to vector
snoc :: Arity n => a -> ContVecT m n a -> ContVecT m (S n) a
snoc a (ContVecT cont) = ContVecT $ \f -> cont $ apLast f a
{-# INLINE snoc #-}

-- | Reverse order of elements in the vector
reverse :: Arity n => ContVecT m n a -> ContVecT m n a
reverse (ContVecT cont) = ContVecT $ cont . reverseF
{-# INLINE reverse #-}

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



izipWithF :: forall n a b c r. (Arity n)
          => (Int -> a -> b -> c) -> Fun n c r -> Fun n a (Fun n b r)
{-# INLINE izipWithF #-}
izipWithF f (Fun g0) =
  fmap (\v -> Fun $ accum
              (\(T_izip i (a:as) g) b -> T_izip (i+1) as (g $ f i a b)
              )
              (\(T_izip _ _ x) -> x)
              (T_izip 0 v g0 :: (T_izip a c r n))
       ) makeList

izipWithFM :: forall m n a b c r. (Arity n, Monad m)
           => (Int -> a -> b -> m c) -> Fun n c (m r) -> Fun n a (Fun n b (m r))
{-# INLINE izipWithFM #-}
izipWithFM f (Fun g0) =
  fmap (\v -> Fun $ accumM
              (\(T_izip i (a:as) g) b -> do x <- f i a b
                                            return $ T_izip (i+1) as (g x)
              )
              (\(T_izip _ _ x) -> x)
              (return $ T_izip 0 v g0 :: m (T_izip a c (m r) n))
       ) makeList


makeList :: forall n a. Arity n => Fun n a [a]
{-# INLINE makeList #-}
makeList = Fun $ accum
    (\(T_mkList xs) x -> T_mkList (xs . (x:)))
    (\(T_mkList xs) -> xs [])
    (T_mkList id :: T_mkList a n)

newtype T_mkList a n = T_mkList ([a] -> [a])

data T_izip a c r n = T_izip Int [a] (Fn n c r)



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

-- | Run continuation vector. It's same as 'inspect' but with
--   arguments flipped.
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

-- | Finalizer function for getting head of the vector.
head :: forall n a. Arity (S n) => Fun (S n) a a
{-# INLINE head #-}
head = Fun $ accum (\(T_head m) a -> T_head $ case m of { Nothing -> Just a; x -> x })
                   (\(T_head (Just x)) -> x)
                   (T_head Nothing :: T_head a (S n))

data T_head a n = T_head (Maybe a)

-- | /O(n)/ Get value at specified index.
index :: forall n a. Arity n => Int -> Fun n a a
index n
  | n < 0     = error "Data.Vector.Fixed.Cont.index: index out of range"
  | otherwise = Fun $ accum
     (\(T_Index x) a -> T_Index $ case x of
                          Left  0 -> Right a
                          Left  i -> Left (i - 1)
                          r       -> r
     )
     (\(T_Index x) -> case x of
                        Left  _ -> error "Data.Vector.Fixed.index: index out of range"
                        Right a -> a
     )
     ( T_Index (Left n) :: T_Index a n)

newtype T_Index a n = T_Index (Either Int a)


-- | Twan van Laarhoven lens for continuation based vector
element :: (Arity n, Functor f)
        => Int -> (a -> f a) -> ContVec n a -> f (ContVec n a)
{-# INLINE element #-}
element i f v = inspect v
              $ elementF i f mkN

-- | Twan van Laarhoven's lens for element of vector with statically
--   known index.
elementTy :: (Arity n, Index k n, Functor f)
          => k -> (a -> f a) -> ContVec n a -> f (ContVec n a)
{-# INLINE elementTy #-}
elementTy k f v = inspect v
                $ lensF k f mkN


-- | Helper for implementation of Twan van Laarhoven lens.
elementF :: forall a n f r. (Arity n, Functor f)
         => Int -> (a -> f a) -> Fun n a r -> Fun n a (f r)
elementF n f (Fun fun0) = Fun $ accum step fini start
  where
    step :: forall k. T_lens f a r (S k) -> a -> T_lens f a r k
    step (T_lens (Left (0,fun))) a = T_lens $ Right $ fmap fun $ f a
    step (T_lens (Left (i,fun))) a = T_lens $ Left (i-1, fun a)
    step (T_lens (Right fun))    a = T_lens $ Right $ fmap ($ a) fun
    --
    fini :: T_lens f a r Z -> f r
    fini (T_lens (Left  _)) = error "Data.Vector.Fixed.lensF: Index out of range"
    fini (T_lens (Right r)) = r 
    --
    start :: T_lens f a r n
    start = T_lens $ Left (n,fun0)

data T_lens f a r n = T_lens (Either (Int,(Fn n a r)) (f (Fn n a r)))



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
-- Deforestation
----------------------------------------------------------------

-- Deforestation uses following assertion: if we convert continuation
-- to vector and immediately back to the continuation we can eliminate
-- intermediate vector. This optimization can however turn
-- nonterminating programs into terminating.
--
-- > runContVec head $ cvec $ vector $ mk2 () ⊥
--
-- If intermediate vector is strict in its elements expression above
-- evaluates to ⊥ too. But if we apply rewrite rule resuling expression:
--
-- > runContVec head $ mk2 () ⊥
--
-- will evaluate to () since ContVec is not strict in its elements.
-- It has been considered acceptable.
--
--
-- In order to get rule fire reliably (it still doesn't). `vector' in
-- inlined starting from phase 1. `cvec' is inlined even later (only
-- during phase 0) because it need to participate in rewriting of
-- indexing functions.


{-# RULES
"cvec/vector" forall v.
  cvec (vector v) = changeMonad runID v
  #-}

 
----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

type instance Dim Complex = N2

instance RealFloat a => Vector Complex a where
  construct = Fun (:+)
  inspect (x :+ y) (Fun f) = f x y


type instance Dim ((,) a) = N2

-- | Note this instance (and other instances for tuples) is
--   essentially monomorphic in element type. Vector type of tuple
--   @(Int,Int)@ is @(,) Int@ so it will only work elements of type
--   @Int@.
instance (b~a) => Vector ((,) b) a where
  construct = Fun (,)
  inspect (a,b) (Fun f) = f a b


type instance Dim ((,,) a b) = N3

instance (b~a, c~a) => Vector ((,,) b c) a where
  construct = Fun (,,)
  inspect (a,b,c) (Fun f) = f a b c


type instance Dim ((,,,) a b c) = N4

instance (b~a, c~a, d~a) => Vector ((,,,) b c d) a where
  construct = Fun (,,,)
  inspect (a,b,c,d) (Fun f) = f a b c d


type instance Dim ((,,,,) a b c d) = N5

instance (b~a, c~a, d~a, e~a) => Vector ((,,,,) b c d e) a where
  construct = Fun (,,,,)
  inspect (a,b,c,d,e) (Fun f) = f a b c d e


type instance Dim ((,,,,,) a b c d e) = N6

instance (b~a, c~a, d~a, e~a, f~a) => Vector ((,,,,,) b c d e f) a where
  construct = Fun (,,,,,)
  inspect (a,b,c,d,e,f) (Fun fun) = fun a b c d e f


type instance Dim ((,,,,,,) a b c d e f) = S N6

instance (b~a, c~a, d~a, e~a, f~a, g~a) => Vector ((,,,,,,) b c d e f g) a where
  construct = Fun (,,,,,,)
  inspect (a,b,c,d,e,f,g) (Fun fun) = fun a b c d e f g
