{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE Rank2Types            #-}
-- |
-- Continuations-based API
module Data.Vector.Fixed.Cont (
    -- * Vector type class
    Dim
  , Vector(..)
  , VectorN
  , length
    -- * Vector as continuation
  , ContVecT(..)
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

import Control.Applicative (Applicative(..))
import Data.Complex        (Complex(..))
import Data.Vector.Fixed.Internal.Arity
import Data.Vector.Fixed.Internal.Id
import Prelude hiding ( replicate,map,zipWith,maximum,minimum,and,or,any,all
                      , foldl,foldr,foldl1,length,sum,reverse
                      , head,tail,mapM,mapM_,sequence,sequence_
                      )


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
  getF :: k -> Fun n a a
  putF :: k -> a -> Fun n a a -> Fun n a a

instance Arity n => Index Z (S n) where
  getF _           = Fun $ \(a :: a) -> unFun (pure a :: Fun n a a)
  putF _ x (Fun f) = Fun $ \_ -> f x

instance Index k n => Index (S k) (S n) where
  getF _           = Fun $ \(_::a) -> unFun (getF (undefined :: k) :: Fun n a a)
  putF _ x (Fun f) = Fun $ \(a::a) -> unFun $ putF (undefined :: k) x (Fun (f a) :: Fun n a a)



----------------------------------------------------------------
-- Cont. vectors and their instances
----------------------------------------------------------------

-- | Vector represented as continuation. Alternative wording: it's
--   Church encoding of N-ary vector.
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

-- | N-ary constructor for vectors. It's wrapped into 'Fun' so that
--   GHC could instantiate it to concrete types.
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
