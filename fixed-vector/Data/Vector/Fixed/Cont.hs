{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, TypeOperators, UndecidableInstances #-}
-- |
-- API for Church-encoded vectors. Implementation of function from
-- "Data.Vector.Fixed" module uses these function internally in order
-- to provide shortcut fusion.
module Data.Vector.Fixed.Cont (
    -- * Type-level numbers
    S
  , Z
  , Add
    -- ** Isomorphism between Peano number and Nats
    -- $natiso
  , NatIso
  , ToPeano
  , ToNat
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
  , WitSum(..)
    -- ** Combinators
  , constFun
  , curryFirst
  , uncurryFirst
  , curryLast
  , curryMany
  , uncurryMany
  , apLast
  , shuffleFun
  , withFun
    -- * Vector type class
  , Dim
  , Vector(..)
  , VectorN
  , length
  , Index(..)
    -- * Vector as continuation
  , ContVec(..)
    -- * Construction of ContVec
  , cvec
  , fromList
  , fromList'
  , fromListM
  , toList
  , replicate
  , replicateM
  , generate
  , generateM
  , unfoldr
  , basis
    -- ** Constructors
  , empty
  , cons
  , consV
  , snoc
  , concat
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
  , mapM_
  , imapM_
  , scanl
  , scanl1
  , sequence
  , sequence_
  , distribute
  , collect
  , distributeM
  , collectM
  , tail
  , reverse
    -- ** Zips
  , zipWith
  , zipWith3
  , izipWith
  , izipWith3
  , zipWithM
  , zipWithM_
  , izipWithM
  , izipWithM_
    -- * Running ContVec
  , runContVec
    -- ** Getters
  , head
  , index
  , element
  , elementTy
    -- ** Vector construction
  , vector
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
  , find
    -- ** Data.Data.Data
  , gfoldl
  , gunfold
  ) where

import Control.Applicative (Applicative(..),(<$>),(<|>))
import Control.Monad       (liftM)
import Data.Coerce
import Data.Complex        (Complex(..))
import Data.Data           (Typeable,Data)
import Data.Typeable       (Proxy(..))
import GHC.TypeLits
import qualified Data.Foldable    as F
import qualified Data.Traversable as F

import Prelude hiding ( replicate,map,zipWith,zipWith3,maximum,minimum,and,or,any,all
                      , foldl,foldr,foldl1,length,sum,reverse,scanl,scanl1
                      , head,tail,mapM,mapM_,sequence,sequence_,concat
                      )


----------------------------------------------------------------
-- Naturals
----------------------------------------------------------------

-- | Type level zero
data Z   deriving Typeable
-- | Successor of n
data S n deriving Typeable

-- | Type family for sum of unary natural numbers.
type family Add n m :: *

type instance Add  Z    n = n
type instance Add (S n) k = S (Add n k)

type N1 = S Z
type N2 = S N1
type N3 = S N2
type N4 = S N3
type N5 = S N4
type N6 = S N5


-- $natiso
--
-- It's only become possible to define isomorphism between Peano
-- number and built-in @Nat@ number in GHC 7.8. It's however
-- impossible to define their properties inductively. So Peano number
-- are used everywhere.

-- | Isomorphism between two representations of natural numbers
class (ToNat a ~ b, ToPeano b ~ a) => NatIso (a :: *) (b :: Nat) where

-- | Convert Peano number to Nat
type family ToNat   (a :: *  ) :: Nat where
  ToNat  Z    = 0
  ToNat (S k) = 1 + ToNat k

-- | Convert Nat number to Peano represenation
type family ToPeano (b :: Nat) :: * where
  ToPeano 0 = Z
  ToPeano n = S (ToPeano (n - 1))

instance NatIso  Z 0 where
instance ( NatIso k (n - 1)
         , ToPeano (n - 1) ~ k
         , ToPeano  n    ~ S k
         , n ~ (1 + (n - 1))    -- n is positive
         ) => NatIso (S k) n where



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
  fmap f fun
     = accum (\(T_Flip g) a -> T_Flip (curryFirst g a))
             (\(T_Flip x)   -> f (unFun x))
             (T_Flip fun)
  {-# INLINE fmap #-}

instance Arity n => Applicative (Fun n a) where
  pure x = accum (\Proxy _ -> Proxy)
                 (\Proxy   -> x)
                  Proxy
  (Fun f0 :: Fun n a (p -> q)) <*> (Fun g0 :: Fun n a p)
    = accum (\(T_ap f g) a -> T_ap (f a) (g a))
            (\(T_ap f g)   -> f g)
            (T_ap f0 g0 :: T_ap a (p -> q) p n)
  {-# INLINE pure  #-}
  {-# INLINE (<*>) #-}

instance Arity n => Monad (Fun n a) where
  return  = pure
  f >>= g = shuffleFun g <*> f
  {-# INLINE return #-}
  {-# INLINE (>>=)  #-}


data    T_ap   a b c n = T_ap (Fn n a b) (Fn n a c)



----------------------------------------------------------------
-- Generic operations of N-ary functions
----------------------------------------------------------------

-- | Type class for handling /n/-ary functions.
class Arity n where
  -- | Left fold over /n/ elements exposed as n-ary function. These
  --   elements are supplied as arguments to the function.
  accum :: (forall k. t (S k) -> a -> t k) -- ^ Fold function
        -> (t Z -> b)                      -- ^ Extract result of fold
        -> t n                             -- ^ Initial value
        -> Fun n a b                       -- ^ Reduction function

  -- | Apply all parameters to the function.
  applyFun :: (forall k. t (S k) -> (a, t k)) -- ^ Get value to apply to function
           -> t n                             -- ^ Initial value
           -> Fn n a b                        -- ^ N-ary function
           -> (b, t Z)

  -- | Apply all parameters to the function using monadic
  --   actions. Note that for identity monad it's same as
  --   applyFun. Ignoring newtypes:
  --
  -- > forall b. Fn n a b -> b  ~ ContVecn n a
  applyFunM :: Monad m
              => (forall k. t (S k) -> m (a, t k)) -- ^ Get value to apply to function
              -> t n                               -- ^ Initial value
              -> m (ContVec n a, t Z)
  -- | Arity of function.
  arity :: n -> Int

  -- | Reverse order of parameters.
  reverseF :: Fun n a b -> Fun n a b
  -- | Worker function for 'gunfold'
  gunfoldF :: (Data a)
           => (forall b x. Data b => c (b -> x) -> c x)
           -> T_gunfold c r a n -> c r
  -- | Proof that `Fn (n+k) a b ~ Fn n a (Fn k a b)`
  witSum :: WitSum n k a b


newtype T_gunfold c r a n = T_gunfold (c (Fn n a r))

-- | Value that carry proof that `Fn (Add n k) a b ~ Fn n a (Fn k a b)`
data WitSum n k a b where
  WitSum :: (Fn (Add n k) a b ~ Fn n a (Fn k a b)) => WitSum n k a b


-- | Apply all parameters to the function.
apply :: Arity n
      => (forall k. t (S k) -> (a, t k)) -- ^ Get value to apply to function
      -> t n                             -- ^ Initial value
      -> ContVec n a                     -- ^ N-ary function
{-# INLINE apply #-}
apply step z = ContVec $ \(Fun f) -> fst $ applyFun step z f

-- | Apply all parameters to the function using monadic actions.
applyM :: (Monad m, Arity n)
       => (forall k. t (S k) -> m (a, t k)) -- ^ Get value to apply to function
       -> t n                               -- ^ Initial value
       -> m (ContVec n a)
{-# INLINE applyM #-}
applyM f t = do (v,_) <- applyFunM f t
                return v

instance Arity Z where
  accum     _ g t = Fun $ g t
  applyFun  _ t h = (h,t)
  applyFunM _ t   = return (empty, t)
  arity  _ = 0
  {-# INLINE accum     #-}
  {-# INLINE applyFun  #-}
  {-# INLINE applyFunM #-}
  {-# INLINE arity     #-}
  reverseF = id
  gunfoldF _ (T_gunfold c) = c
  {-# INLINE reverseF #-}
  {-# INLINE gunfoldF #-}
  witSum = WitSum
  {-# INLINE witSum #-}

instance Arity n => Arity (S n) where
  accum     f g t = Fun $ \a -> unFun $ accum f g (f t a)
  applyFun  f t h = case f t of (a,u) -> applyFun f u (h a)
  applyFunM f t   = do (a,t')   <- f t
                       (vec,tZ) <- applyFunM f t'
                       return (cons a vec , tZ)
  arity    _ = 1 + arity (undefined :: n)
  {-# INLINE accum     #-}
  {-# INLINE applyFun  #-}
  {-# INLINE applyFunM #-}
  {-# INLINE arity     #-}
  reverseF f   = Fun $ \a -> unFun (reverseF $ apLast f a)
  gunfoldF f c = gunfoldF f (apGunfold f c)
  {-# INLINE reverseF #-}
  {-# INLINE gunfoldF #-}
  witSum = witSumWorker
  {-# INLINE witSum #-}

witSumWorker :: forall n k a b. Arity n => WitSum (S n) k a b
{-# INLINE witSumWorker #-}
witSumWorker = case witSum :: WitSum n k a b of
                 WitSum -> WitSum

apGunfold :: Data a
          => (forall b x. Data b => c (b -> x) -> c x)
          -> T_gunfold c r a (S n)
          -> T_gunfold c r a n
apGunfold f (T_gunfold c) = T_gunfold $ f c
{-# INLINE apGunfold #-}


newtype T_Flip    a b n = T_Flip (Fun n a b)
newtype T_Counter n     = T_Counter Int



----------------------------------------------------------------
-- Combinators
----------------------------------------------------------------

-- | Prepend ignored parameter to function
constFun :: Fun n a b -> Fun (S n) a b
constFun (Fun f) = Fun $ \_ -> f
{-# INLINE constFun #-}

-- | Curry first parameter of n-ary function
curryFirst :: Fun (S n) a b -> a -> Fun n a b
curryFirst = coerce
{-# INLINE curryFirst #-}

-- | Uncurry first parameter of n-ary function
uncurryFirst :: (a -> Fun n a b) -> Fun (S n) a b
uncurryFirst = coerce
{-# INLINE uncurryFirst #-}

-- | Curry last parameter of n-ary function
curryLast :: Arity n => Fun (S n) a b -> Fun n a (a -> b)
{-# INLINE curryLast #-}
curryLast (Fun f0) = accum (\(T_fun f) a -> T_fun (f a))
                           (\(T_fun f)   -> f)
                           (T_fun f0)

newtype T_fun a b n = T_fun (Fn (S n) a b)

-- | Curry /n/ first parameters of n-ary function
curryMany :: forall n k a b. Arity n
          => Fun (Add n k) a b -> Fun n a (Fun k a b)
{-# INLINE curryMany #-}
curryMany (Fun f0) = accum
  (\(T_curry f) a -> T_curry (f a))
  (\(T_curry f) -> Fun f :: Fun k a b)
  ( T_curry f0 :: T_curry a b k n)

newtype T_curry a b k n = T_curry (Fn (Add n k) a b)

-- | Uncurry /n/ first parameters of n-ary function
uncurryMany :: forall n k a b. Arity n
            => Fun n a (Fun k a b) -> Fun (Add n k) a b
{-# INLINE uncurryMany #-}
uncurryMany f =
  case witSum :: WitSum n k a b of
    WitSum -> coerce (fmap unFun f :: Fun n a (Fn k a b))



-- | Apply last parameter to function. Unlike 'apFun' we need to
--   traverse all parameters but last hence 'Arity' constraint.
apLast :: Arity n => Fun (S n) a b -> a -> Fun n a b
apLast f x = fmap ($ x) $ curryLast f
{-# INLINE apLast #-}

-- | Recursive step for the function
withFun :: (Fun n a b -> Fun n a b) -> Fun (S n) a b -> Fun (S n) a b
withFun f fun = Fun $ \a -> unFun $ f $ curryFirst fun a
{-# INLINE withFun #-}

-- | Move function parameter to the result of N-ary function.
shuffleFun :: Arity n
           => (b -> Fun n a r) -> Fun n a (b -> r)
{-# INLINE shuffleFun #-}
shuffleFun f0
  = accum (\(T_shuffle f) a -> T_shuffle $ \x -> f x a)
          (\(T_shuffle f)   -> f)
          (T_shuffle (fmap unFun f0))

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
  basicIndex v i = index i (cvec v)
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
  putF  :: k -> a -> Fun n a r -> Fun n a r
  lensF :: Functor f => k -> (a -> f a) -> Fun n a r -> Fun n a (f r)

instance Arity n => Index Z (S n) where
  getF  _       = Fun $ \(a :: a) -> unFun (pure a :: Fun n a a)
  putF  _ a (Fun f) = Fun $ \_ -> f a
  lensF _ f fun = Fun $ \(a :: a) -> unFun $
    (\g -> g <$> f a) <$> shuffleFun (curryFirst fun)
  {-# INLINE getF  #-}
  {-# INLINE putF  #-}
  {-# INLINE lensF #-}

instance Index k n => Index (S k) (S n) where
  getF  _       = Fun $ \(_::a) -> unFun (getF  (undefined :: k) :: Fun n a a)
  putF _ a (f :: Fun (S n) a b)
    = withFun (putF (undefined :: k) a) f
  lensF _ f fun = Fun $ \a -> unFun (lensF (undefined :: k) f (curryFirst fun a))
  {-# INLINE getF  #-}
  {-# INLINE putF  #-}
  {-# INLINE lensF #-}


----------------------------------------------------------------
-- Cont. vectors and their instances
----------------------------------------------------------------

-- | Vector represented as continuation. Alternative wording: it's
--   Church encoded N-element vector.
newtype ContVec n a = ContVec (forall r. Fun n a r -> r)

type instance Dim (ContVec n) = n

instance Arity n => Vector (ContVec n) a where
  construct = accum
    (\(T_mkN f) a -> T_mkN (f . cons a))
    (\(T_mkN f)   -> f empty)
    (T_mkN id)
  inspect (ContVec c) f = c f
  {-# INLINE construct #-}
  {-# INLINE inspect   #-}

newtype T_mkN n_tot a n = T_mkN (ContVec n a -> ContVec n_tot a)

instance Arity n => VectorN ContVec n a


instance (Arity n) => Functor (ContVec n) where
  fmap = map
  {-# INLINE fmap #-}

instance (Arity n) => Applicative (ContVec n) where
  pure  = replicate
  (<*>) = zipWith ($)
  {-# INLINE pure  #-}
  {-# INLINE (<*>) #-}

instance (Arity n) => F.Foldable (ContVec n) where
  foldr = foldr
  {-# INLINE foldr #-}

instance (Arity n) => F.Traversable (ContVec n) where
  sequenceA v = inspect v $ sequenceAF construct
  {-# INLINE sequenceA #-}

sequenceAF :: forall f n a b. (Applicative f, Arity n)
     => Fun n a b -> Fun n (f a) (f b)
{-# INLINE sequenceAF #-}
sequenceAF (Fun f0)
  = accum (\(T_sequenceA f) a -> T_sequenceA (f <*> a))
          (\(T_sequenceA f)   -> f)
          (T_sequenceA (pure f0) :: T_sequenceA f a b n)

newtype T_sequenceA f a b n = T_sequenceA (f (Fn n a b))



----------------------------------------------------------------
-- Construction
----------------------------------------------------------------

-- | Convert regular vector to continuation based one.
cvec :: (Vector v a, Dim v ~ n) => v a -> ContVec n a
cvec v = ContVec (inspect v)
{-# INLINE[0] cvec #-}

-- | Create empty vector.
empty :: ContVec Z a
{-# INLINE empty #-}
empty = ContVec (\(Fun r) -> r)


-- | Convert list to continuation-based vector. Will throw error if
--   list is shorter than resulting vector.
fromList :: forall n a. Arity n => [a] -> ContVec n a
{-# INLINE fromList #-}
fromList xs =
  apply step
        (T_flist xs :: T_flist a n)
  where
    step (T_flist []    ) = error "Data.Vector.Fixed.Cont.fromList: too few elements"
    step (T_flist (a:as)) = (a, T_flist as)

-- | Same as 'fromList' bu throws error is list doesn't have same
--   length as vector.
fromList' :: forall n a. Arity n => [a] -> ContVec n a
{-# INLINE fromList' #-}
fromList' xs = ContVec $ \(Fun fun) ->
  let (r,rest) = applyFun step (T_flist xs :: T_flist a n) fun
      step (T_flist []    ) = error "Data.Vector.Fixed.Cont.fromList': too few elements"
      step (T_flist (a:as)) = (a, T_flist as)
  in case rest of
       T_flist [] -> r
       _          -> error "Data.Vector.Fixed.Cont.fromList': too many elements"

-- | Convert list to continuation-based vector. Will fail with
--   'Nothing' if list doesn't have right length.
fromListM :: forall n a. Arity n => [a] -> Maybe (ContVec n a)
{-# INLINE fromListM #-}
fromListM xs = do
  (v,rest) <- applyFunM step (T_flist xs :: T_flist a n)
  case rest of
    T_flist [] -> return v
    _          -> Nothing
  where
    step (T_flist []    ) = Nothing
    step (T_flist (a:as)) = return (a, T_flist as)

newtype T_flist a n = T_flist [a]


-- | Convert vector to the list
toList :: (Arity n) => ContVec n a -> [a]
toList = foldr (:) []
{-# INLINE toList #-}


-- | Execute monadic action for every element of vector. Synonym for 'pure'.
replicate :: (Arity n) => a -> ContVec n a
{-# INLINE replicate #-}
replicate a = apply (\Proxy -> (a, Proxy)) Proxy

-- | Execute monadic action for every element of vector.
replicateM :: (Arity n, Monad m) => m a -> m (ContVec n a)
{-# INLINE replicateM #-}
replicateM act
  = applyM (\Proxy -> do { a <- act; return (a, Proxy)}) Proxy


-- | Generate vector from function which maps element's index to its value.
generate :: (Arity n) => (Int -> a) -> ContVec n a
{-# INLINE generate #-}
generate f =
  apply (\(T_Counter n) -> (f n, T_Counter (n + 1)))
        (T_Counter 0)

-- | Generate vector from monadic function which maps element's index
--   to its value.
generateM :: (Monad m, Arity n) => (Int -> m a) -> m (ContVec n a)
{-# INLINE generateM #-}
generateM f =
  applyM (\(T_Counter n) -> do { a <- f n; return (a, T_Counter (n + 1)) } )
         (T_Counter 0)


-- | Unfold vector.
unfoldr :: Arity n => (b -> (a,b)) -> b -> ContVec n a
{-# INLINE unfoldr #-}
unfoldr f b0 =
  apply (\(T_unfoldr b) -> let (a,b') = f b in (a, T_unfoldr b'))
        (T_unfoldr b0)

newtype T_unfoldr b n = T_unfoldr b


-- | Unit vector along Nth axis.
basis :: (Num a, Arity n) => Int -> ContVec n a
{-# INLINE basis #-}
basis n0 =
  apply (\(T_Counter n) -> (if n == 0 then 1 else 0, T_Counter (n - 1)))
        (T_Counter n0)



mk1 :: a -> ContVec N1 a
mk1 a1 = ContVec $ \(Fun f) -> f a1
{-# INLINE mk1 #-}

mk2 :: a -> a -> ContVec N2 a
mk2 a1 a2 = ContVec $ \(Fun f) -> f a1 a2
{-# INLINE mk2 #-}

mk3 :: a -> a -> a -> ContVec N3 a
mk3 a1 a2 a3 = ContVec $ \(Fun f) -> f a1 a2 a3
{-# INLINE mk3 #-}

mk4 :: a -> a -> a -> a -> ContVec N4 a
mk4 a1 a2 a3 a4 = ContVec $ \(Fun f) -> f a1 a2 a3 a4
{-# INLINE mk4 #-}

mk5 :: a -> a -> a -> a -> a -> ContVec N5 a
mk5 a1 a2 a3 a4 a5 = ContVec $ \(Fun f) -> f a1 a2 a3 a4 a5
{-# INLINE mk5 #-}



----------------------------------------------------------------
-- Transforming vectors
----------------------------------------------------------------

-- | Map over vector. Synonym for 'fmap'
map :: (Arity n) => (a -> b) -> ContVec n a -> ContVec n b
{-# INLINE map #-}
map = imap . const

-- | Apply function to every element of the vector and its index.
imap :: (Arity n) => (Int -> a -> b) -> ContVec n a -> ContVec n b
{-# INLINE imap #-}
imap f (ContVec contA) = ContVec $
  contA . imapF f

-- | Monadic map over vector.
mapM :: (Arity n, Monad m) => (a -> m b) -> ContVec n a -> m (ContVec n b)
{-# INLINE mapM #-}
mapM = imapM . const

-- | Apply monadic function to every element of the vector and its index.
imapM :: (Arity n, Monad m) => (Int -> a -> m b) -> ContVec n a -> m (ContVec n b)
{-# INLINE imapM #-}
imapM f v
  = inspect v
  $ imapMF f construct

-- | Apply monadic action to each element of vector and ignore result.
mapM_ :: (Arity n, Monad m) => (a -> m b) -> ContVec n a -> m ()
{-# INLINE mapM_ #-}
mapM_ f = foldl (\m a -> m >> f a >> return ()) (return ())

-- | Apply monadic action to each element of vector and its index and
--   ignore result.
imapM_ :: (Arity n, Monad m) => (Int -> a -> m b) -> ContVec n a -> m ()
{-# INLINE imapM_ #-}
imapM_ f = ifoldl (\m i a -> m >> f i a >> return ()) (return ())


imapMF :: (Arity n, Monad m)
       => (Int -> a -> m b) -> Fun n b r -> Fun n a (m r)
{-# INLINE imapMF #-}
imapMF f (Fun funB) =
  accum (\(T_mapM i m) a -> T_mapM (i+1) $ do b   <- f i a
                                              fun <- m
                                              return $ fun b
                           )
        (\(T_mapM _ m) -> m)
        (T_mapM 0 (return funB))

data T_mapM a m r n = T_mapM Int (m (Fn n a r))

imapF :: Arity n
      => (Int -> a -> b) -> Fun n b r -> Fun n a r
{-# INLINE imapF #-}
imapF f (Fun funB) =
  accum (\(T_map i g) b -> T_map (i+1) (g (f i b)))
        (\(T_map _ r)   -> r)
        (  T_map 0 funB)

data T_map a r n = T_map Int (Fn n a r)

-- | Left scan over vector
scanl :: (Arity n) => (b -> a -> b) -> b -> ContVec n a -> ContVec (S n) b
{-# INLINE scanl #-}
scanl f b0 (ContVec cont) = ContVec $
  cont . scanlF f b0

-- | Left scan over vector
scanl1 :: (Arity n) => (a -> a -> a) -> ContVec n a -> ContVec n a
{-# INLINE scanl1 #-}
scanl1 f (ContVec cont) = ContVec $
  cont . scanl1F f

scanlF :: forall n a b r. (Arity n) => (b -> a -> b) -> b -> Fun (S n) b r -> Fun n a r
scanlF f b0 (Fun fun0)
  = accum step fini start
  where
    step  :: forall k. T_scanl r b (S k) -> a -> T_scanl r b k
    step (T_scanl b fn) a = let b' = f b a in T_scanl b' (fn b')
    fini (T_scanl _ r) = r
    start = T_scanl b0 (fun0 b0)  :: T_scanl r b n

scanl1F :: forall n a r. (Arity n) => (a -> a -> a) -> Fun n a r -> Fun n a r
scanl1F f (Fun fun0) = accum step fini start
  where
    step  :: forall k. T_scanl1 r a (S k) -> a -> T_scanl1 r a k
    step (T_scanl1 Nothing  fn) a = T_scanl1 (Just a) (fn a)
    step (T_scanl1 (Just x) fn) a = let a' = f x a in T_scanl1 (Just a') (fn a')
    fini (T_scanl1 _ r) = r
    start = T_scanl1 Nothing fun0 :: T_scanl1 r a n

data T_scanl  r a n = T_scanl a (Fn n a r)
data T_scanl1 r a n = T_scanl1 (Maybe a) (Fn n a r)


-- | Evaluate every action in the vector from left to right.
sequence :: (Arity n, Monad m) => ContVec n (m a) -> m (ContVec n a)
sequence = mapM id
{-# INLINE sequence #-}

-- | Evaluate every action in the vector from left to right and ignore result.
sequence_ :: (Arity n, Monad m) => ContVec n (m a) -> m ()
sequence_ = mapM_ id
{-# INLINE sequence_ #-}

-- | The dual of sequenceA
distribute :: (Functor f, Arity n) => f (ContVec n a) -> ContVec n (f a)
{-# INLINE distribute #-}
distribute f0
  = apply step start
  where
    -- It's not possible to use ContVec as accumulator type since `head'
    -- require Arity constraint on `k'. So we use plain lists
    step (T_distribute f) = ( fmap (\(x:_) -> x) f
                            , T_distribute $ fmap (\(_:x) -> x) f)
    start = T_distribute (fmap toList f0)

collect :: (Functor f, Arity n) => (a -> ContVec n b) -> f a -> ContVec n (f b)
collect f = distribute . fmap f
{-# INLINE collect #-}

-- | The dual of sequence
distributeM :: (Monad m, Arity n) => m (ContVec n a) -> ContVec n (m a)
{-# INLINE distributeM #-}
distributeM f0
  = apply step start
  where
    step (T_distribute f) = ( liftM (\(x:_) -> x) f
                            , T_distribute $ liftM (\(_:x) -> x) f)
    start = T_distribute (liftM toList f0)

collectM :: (Monad m, Arity n) => (a -> ContVec n b) -> m a -> ContVec n (m b)
collectM f = distributeM . liftM f
{-# INLINE collectM #-}

newtype T_distribute a f n = T_distribute (f [a])


-- | /O(1)/ Tail of vector.
tail :: ContVec (S n) a -> ContVec n a
tail (ContVec cont) = ContVec $ \f -> cont $ constFun f
{-# INLINE tail #-}

-- | /O(1)/ Prepend element to vector
cons :: a -> ContVec n a -> ContVec (S n) a
cons a (ContVec cont) = ContVec $ \f -> cont $ curryFirst f a
{-# INLINE cons #-}

-- | Prepend single element vector to another vector.
consV :: ContVec (S Z) a -> ContVec n a -> ContVec (S n) a
{-# INLINE consV #-}
consV (ContVec cont1) (ContVec cont)
  = ContVec $ \f -> cont $ curryFirst f $ cont1 $ Fun id

-- | /O(1)/ Append element to vector
snoc :: Arity n => a -> ContVec n a -> ContVec (S n) a
snoc a (ContVec cont) = ContVec $ \f -> cont $ apLast f a
{-# INLINE snoc #-}

-- | Concatenate vector
concat :: (Arity n, Arity k, Arity (Add n k))
       => ContVec n a -> ContVec k a -> ContVec (Add n k) a
{-# INLINE concat #-}
concat v u = inspect u
           $ inspect v
           $ curryMany construct

-- | Reverse order of elements in the vector
reverse :: Arity n => ContVec n a -> ContVec n a
reverse (ContVec cont) = ContVec $ cont . reverseF
{-# INLINE reverse #-}

-- | Zip two vector together using function.
zipWith :: (Arity n) => (a -> b -> c)
        -> ContVec n a -> ContVec n b -> ContVec n c
{-# INLINE zipWith #-}
zipWith = izipWith . const

-- | Zip three vectors together
zipWith3 :: (Arity n) => (a -> b -> c -> d)
         -> ContVec n a -> ContVec n b -> ContVec n c -> ContVec n d
{-# INLINE zipWith3 #-}
zipWith3 f v1 v2 v3 = zipWith (\a (b, c) -> f a b c) v1 (zipWith (,) v2 v3)

-- | Zip two vector together using function which takes element index
--   as well.
izipWith :: (Arity n) => (Int -> a -> b -> c)
         -> ContVec n a -> ContVec n b -> ContVec n c
{-# INLINE izipWith #-}
izipWith f vecA vecB = ContVec $ \funC ->
    inspect vecB
  $ inspect vecA
  $ izipWithF f funC

-- | Zip three vectors together
izipWith3 :: (Arity n) => (Int -> a -> b -> c -> d)
          -> ContVec n a -> ContVec n b -> ContVec n c -> ContVec n d
{-# INLINE izipWith3 #-}
izipWith3 f v1 v2 v3 = izipWith (\i a (b, c) -> f i a b c) v1 (zipWith (,) v2 v3)

-- | Zip two vector together using monadic function.
zipWithM :: (Arity n, Monad m) => (a -> b -> m c)
         -> ContVec n a -> ContVec n b -> m (ContVec n c)
{-# INLINE zipWithM #-}
zipWithM f v w = sequence $ zipWith f v w

zipWithM_ :: (Arity n, Monad m)
          => (a -> b -> m c) -> ContVec n a -> ContVec n b -> m ()
{-# INLINE zipWithM_ #-}
zipWithM_ f xs ys = sequence_ (zipWith f xs ys)

-- | Zip two vector together using monadic function which takes element
--   index as well..
izipWithM :: (Arity n, Monad m) => (Int -> a -> b -> m c)
          -> ContVec n a -> ContVec n b -> m (ContVec n c)
{-# INLINE izipWithM #-}
izipWithM f v w = sequence $ izipWith f v w

izipWithM_ :: (Arity n, Monad m)
           => (Int -> a -> b -> m c) -> ContVec n a -> ContVec n b -> m ()
{-# INLINE izipWithM_ #-}
izipWithM_ f xs ys = sequence_ (izipWith f xs ys)

izipWithF :: (Arity n)
          => (Int -> a -> b -> c) -> Fun n c r -> Fun n a (Fun n b r)
{-# INLINE izipWithF #-}
izipWithF f (Fun g0) =
  fmap (\v -> accum
              (\(T_izip i (a:as) g) b -> T_izip (i+1) as (g $ f i a b))
              (\(T_izip _ _      x)   -> x)
              (T_izip 0 v g0)
       ) makeList


makeList :: Arity n => Fun n a [a]
{-# INLINE makeList #-}
makeList = accum
    (\(T_mkList xs) x -> T_mkList (xs . (x:)))
    (\(T_mkList xs) -> xs [])
    (T_mkList id)

newtype T_mkList a n = T_mkList ([a] -> [a])

data T_izip a c r n = T_izip Int [a] (Fn n c r)



----------------------------------------------------------------
-- Running vector
----------------------------------------------------------------

-- | Run continuation vector. It's same as 'inspect' but with
--   arguments flipped.
runContVec :: Fun n a r
           -> ContVec n a
           -> r
runContVec f (ContVec c) = c f
{-# INLINE runContVec #-}

-- | Convert continuation to the vector.
vector :: (Vector v a, Dim v ~ n) => ContVec n a -> v a
vector = runContVec construct
{-# INLINE[1] vector #-}

-- | Finalizer function for getting head of the vector.
head :: Arity (S n) => ContVec (S n) a -> a
-- NOTE: we need constraint `Arity (S n)' instead of `Arity n' because
--       `Vector v' entails `Arity (Dim v)' and GHC cannot figure out
--       that `Arity (S n)' ⇒ `Arity n'
{-# INLINE head #-}
head
  = runContVec
  $ accum (\(T_head m) a -> T_head $ case m of { Nothing -> Just a; x -> x })
          (\(T_head (Just x)) -> x)
          (T_head Nothing)

data T_head a n = T_head (Maybe a)


-- | /O(n)/ Get value at specified index.
index :: Arity n => Int -> ContVec n a -> a
{-# INLINE index #-}
index n
  | n < 0     = error "Data.Vector.Fixed.Cont.index: index out of range"
  | otherwise = runContVec $ accum
     (\(T_Index x) a -> T_Index $ case x of
                          Left  0 -> Right a
                          Left  i -> Left (i - 1)
                          r       -> r
     )
     (\(T_Index x) -> case x of
                        Left  _ -> error "Data.Vector.Fixed.index: index out of range"
                        Right a -> a
     )
     (T_Index (Left n))

newtype T_Index a n = T_Index (Either Int a)


-- | Twan van Laarhoven lens for continuation based vector
element :: (Arity n, Functor f)
        => Int -> (a -> f a) -> ContVec n a -> f (ContVec n a)
{-# INLINE element #-}
element i f v = inspect v
              $ elementF i f construct

-- | Twan van Laarhoven's lens for element of vector with statically
--   known index.
elementTy :: (Arity n, Index k n, Functor f)
          => k -> (a -> f a) -> ContVec n a -> f (ContVec n a)
{-# INLINE elementTy #-}
elementTy k f v = inspect v
                $ lensF k f construct


-- | Helper for implementation of Twan van Laarhoven lens.
elementF :: forall a n f r. (Arity n, Functor f)
         => Int -> (a -> f a) -> Fun n a r -> Fun n a (f r)
{-# INLINE elementF #-}
elementF n f (Fun fun0) = accum step fini start
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
foldl :: Arity n => (b -> a -> b) -> b -> ContVec n a -> b
{-# INLINE foldl #-}
foldl f = ifoldl (\b _ a -> f b a)

-- | Left fold over continuation vector.
ifoldl :: forall n a b. Arity n
       => (b -> Int -> a -> b) -> b -> ContVec n a -> b
{-# INLINE ifoldl #-}
ifoldl f b v
  = inspect v
  $ accum (\(T_ifoldl i r) a -> T_ifoldl (i+1) (f r i a))
          (\(T_ifoldl _ r) -> r)
          (T_ifoldl 0 b :: T_ifoldl b n)

-- | Monadic left fold over continuation vector.
foldM :: (Arity n, Monad m)
      => (b -> a -> m b) -> b -> ContVec n a -> m b
{-# INLINE foldM #-}
foldM f x
  = foldl (\m a -> do{ b <- m; f b a}) (return x)

-- | Monadic left fold over continuation vector.
ifoldM :: (Arity n, Monad m)
       => (b -> Int -> a -> m b) -> b -> ContVec n a -> m b
{-# INLINE ifoldM #-}
ifoldM f x
  = ifoldl (\m i a -> do{ b <- m; f b i a}) (return x)

data T_ifoldl b n = T_ifoldl !Int b

-- Implementation of foldl1 is quite ugly. It could be expressed in
-- terms of foldlF (worker function for foldl)
--
-- > foldl1F f = Fun $ \a -> case foldlF f a :: Fun n a a of Fun g -> g
--
-- But it require constraint `Arity n` whereas `Vector v a` gives
-- `Arity (S n)`.  Latter imply former but GHC cannot infer it.

-- | Left fold.
foldl1 :: (Arity (S n)) => (a -> a -> a) -> ContVec (S n) a -> a
{-# INLINE foldl1 #-}
foldl1 f
  = runContVec
  $ accum (\(T_foldl1 r       ) a -> T_foldl1 $ Just $ maybe a (flip f a) r)
          (\(T_foldl1 (Just x))   -> x)
          (T_foldl1 Nothing)

newtype T_foldl1 a n = T_foldl1 (Maybe a)

-- | Right fold over continuation vector
foldr :: Arity n => (a -> b -> b) -> b -> ContVec n a -> b
{-# INLINE foldr #-}
foldr = ifoldr . const

-- | Right fold over continuation vector
ifoldr :: Arity n => (Int -> a -> b -> b) -> b -> ContVec n a -> b
{-# INLINE ifoldr #-}
ifoldr f z
  = runContVec
  $ accum (\(T_ifoldr i g) a -> T_ifoldr (i+1) (g . f i a))
          (\(T_ifoldr _ g)   -> g z)
          (T_ifoldr 0 id)

data T_ifoldr b n = T_ifoldr Int (b -> b)

-- | Sum all elements in the vector.
sum :: (Num a, Arity n) => ContVec n a -> a
sum = foldl (+) 0
{-# INLINE sum #-}

-- | Minimal element of vector.
minimum :: (Ord a, Arity (S n)) => ContVec (S n) a -> a
minimum = foldl1 min
{-# INLINE minimum #-}

-- | Maximal element of vector.
maximum :: (Ord a, Arity (S n)) => ContVec (S n) a -> a
maximum = foldl1 max
{-# INLINE maximum #-}

-- | Conjunction of elements of a vector.
and :: Arity n => ContVec n Bool -> Bool
and = foldr (&&) True
{-# INLINE and #-}

-- | Disjunction of all elements of a vector.
or :: Arity n => ContVec n Bool -> Bool
or = foldr (||) False
{-# INLINE or #-}

-- | Determines whether all elements of vector satisfy predicate.
all :: Arity n => (a -> Bool) -> ContVec n a -> Bool
all f = foldr (\x b -> f x && b) True
{-# INLINE all #-}

-- | Determines whether any of element of vector satisfy predicate.
any :: Arity n => (a -> Bool) -> ContVec n a -> Bool
any f = foldr (\x b -> f x && b) True
{-# INLINE any #-}

-- | The 'find' function takes a predicate and a vector and returns
--   the leftmost element of the vector matching the predicate,
--   or 'Nothing' if there is no such element.
find :: Arity n => (a -> Bool) -> ContVec n a -> Maybe a
find f = foldl (\r x -> r <|> if f x then Just x else Nothing) Nothing
{-# INLINE find #-}

-- | Generic 'Data.Data.gfoldl' which could work with any vector.
gfoldl :: forall c v a. (Vector v a, Data a)
       => (forall x y. Data x => c (x -> y) -> x -> c y)
       -> (forall x  . x -> c x)
       -> v a -> c (v a)
gfoldl f inj v
  = inspect v
  $ gfoldlF f (inj $ unFun (construct :: Fun (Dim v) a (v a)))

-- | Generic 'Data.Data.gunfoldl' which could work with any
--   vector. Since vector can only have one constructor argument for
--   constructor is ignored.
gunfold :: forall con c v a. (Vector v a, Data a)
        => (forall b r. Data b => c (b -> r) -> c r)
        -> (forall r. r -> c r)
        -> con -> c (v a)
gunfold f inj _
  = gunfoldF f gun
  where
    con = construct                   :: Fun (Dim v) a (v a)
    gun = T_gunfold (inj $ unFun con) :: T_gunfold c (v a) a (Dim v)


gfoldlF :: (Arity n, Data a)
         => (forall x y. Data x => c (x -> y) -> x -> c y)
         -> c (Fn n a r) -> Fun n a (c r)
gfoldlF f c0 = accum
  (\(T_gfoldl c) x -> T_gfoldl (f c x))
  (\(T_gfoldl c)   -> c)
  (T_gfoldl   c0)

newtype T_gfoldl c r a n = T_gfoldl (c (Fn n a r))



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
  cvec (vector v) = v
  #-}


----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

type instance Dim Complex = N2

instance RealFloat a => Vector Complex a where
  construct = Fun (:+)
  inspect (x :+ y) (Fun f) = f x y
  {-# INLINE construct #-}
  {-# INLINE inspect #-}


type instance Dim ((,) a) = N2

-- | Note this instance (and other instances for tuples) is
--   essentially monomorphic in element type. Vector type /v/ of 2
--   element tuple @(Int,Int)@ is @(,) Int@ so it will only work
--   with elements of type @Int@.
instance (b~a) => Vector ((,) b) a where
  construct = Fun (,)
  inspect (a,b) (Fun f) = f a b
  {-# INLINE construct #-}
  {-# INLINE inspect #-}


type instance Dim ((,,) a b) = N3

instance (b~a, c~a) => Vector ((,,) b c) a where
  construct = Fun (,,)
  inspect (a,b,c) (Fun f) = f a b c
  {-# INLINE construct #-}
  {-# INLINE inspect #-}


type instance Dim ((,,,) a b c) = N4

instance (b~a, c~a, d~a) => Vector ((,,,) b c d) a where
  construct = Fun (,,,)
  inspect (a,b,c,d) (Fun f) = f a b c d
  {-# INLINE construct #-}
  {-# INLINE inspect #-}


type instance Dim ((,,,,) a b c d) = N5

instance (b~a, c~a, d~a, e~a) => Vector ((,,,,) b c d e) a where
  construct = Fun (,,,,)
  inspect (a,b,c,d,e) (Fun f) = f a b c d e
  {-# INLINE construct #-}
  {-# INLINE inspect #-}


type instance Dim ((,,,,,) a b c d e) = N6

instance (b~a, c~a, d~a, e~a, f~a) => Vector ((,,,,,) b c d e f) a where
  construct = Fun (,,,,,)
  inspect (a,b,c,d,e,f) (Fun fun) = fun a b c d e f
  {-# INLINE construct #-}
  {-# INLINE inspect #-}


type instance Dim ((,,,,,,) a b c d e f) = S N6

instance (b~a, c~a, d~a, e~a, f~a, g~a) => Vector ((,,,,,,) b c d e f g) a where
  construct = Fun (,,,,,,)
  inspect (a,b,c,d,e,f,g) (Fun fun) = fun a b c d e f g
  {-# INLINE construct #-}
  {-# INLINE inspect #-}

type instance Dim Proxy = Z

instance Vector Proxy a where
  construct = Fun Proxy
  inspect _ = unFun

