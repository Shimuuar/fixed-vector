{-# LANGUAGE CPP                  #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- API for Church-encoded vectors. Implementation of function from
-- "Data.Vector.Fixed" module uses these function internally in order
-- to provide shortcut fusion.
module Data.Vector.Fixed.Cont (
    -- * Type-level numbers
    PeanoNum(..)
  , N1,N2,N3,N4,N5,N6,N7,N8
  , Peano
  , Add
    -- * N-ary functions
  , Fn
  , Fun(..)
  , Arity
  , ArityPeano(..)
  , apply
  , applyM
  , Index(..)
    -- ** Combinators
  , constFun
  , curryFirst
  , uncurryFirst
  , curryLast
  , curryMany
  , apLast
  , shuffleFun
  , withFun
    -- * Vector type class
  , Dim
  , Vector(..)
  , length
    -- * Vector as continuation
  , ContVec(..)
  , consPeano
  , runContVec
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
  , mk6
  , mk7
  , mk8
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
    -- ** Getters
  , head
  , index
  , element
    -- ** Vector construction
  , vector
    -- ** Folds
  , foldl
  , foldl'
  , foldl1
  , foldr
  , ifoldl
  , ifoldl'
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

import Control.Applicative   ((<|>), Const(..))
import Data.Coerce
import Data.Complex          (Complex(..))
import Data.Data             (Data)
import Data.Kind             (Type)
import Data.Functor.Identity (Identity(..))
import Data.Typeable         (Proxy(..))
import qualified Data.Foldable    as F
import qualified Data.Traversable as T
import Unsafe.Coerce       (unsafeCoerce)
import GHC.TypeLits
import GHC.Exts       (Proxy#, proxy#)
import Prelude        ( Bool(..), Int, Maybe(..), Either(..)
                      , Eq(..), Ord(..), Num(..), Functor(..), Applicative(..), Monad(..)
                      , Semigroup(..), Monoid(..)
                      , (.), ($), (&&), (||), (<$>), const, id, flip, error, otherwise, fst, maybe
                      )


----------------------------------------------------------------
-- Naturals
----------------------------------------------------------------

-- | Peano numbers. Since type level naturals don't support induction
--   we have to convert type nats to Peano representation first and
--   work with it,
data PeanoNum = Z
              | S PeanoNum

type N1 = S Z
type N2 = S N1
type N3 = S N2
type N4 = S N3
type N5 = S N4
type N6 = S N5
type N7 = S N6
type N8 = S N7


-- | Convert type level natural to Peano representation
type family Peano (n :: Nat) :: PeanoNum where
  Peano 0 = 'Z
  Peano n = 'S (Peano (n - 1))

-- | Type family for sum of unary natural numbers.
type family Add (n :: PeanoNum) (m :: PeanoNum) :: PeanoNum where
  Add  'Z    n = n
  Add ('S n) k = 'S (Add n k)


----------------------------------------------------------------
-- N-ary functions
----------------------------------------------------------------

-- | Type family for n-ary functions. @n@ is number of parameters of
--   type @a@ and @b@ is result type.
type family Fn (n :: PeanoNum) (a :: Type) (b :: Type) where
  Fn 'Z     a b = b
  Fn ('S n) a b = a -> Fn n a b

-- | Newtype wrapper which is used to make 'Fn' injective. It's a
--   function which takes @n@ parameters of type @a@ and returns value
--   of type @b@.
newtype Fun n a b = Fun { unFun :: Fn n a b }


instance ArityPeano n => Functor (Fun n a) where
  fmap f fun
     = accum (\(T_Flip g) a -> T_Flip (curryFirst g a))
             (\(T_Flip x)   -> f (unFun x))
             (T_Flip fun)
  {-# INLINE fmap #-}

instance ArityPeano n => Applicative (Fun n a) where
  pure x = accum (\Proxy _ -> Proxy)
                 (\Proxy   -> x)
                  Proxy
  (Fun f0 :: Fun n a (p -> q)) <*> (Fun g0 :: Fun n a p)
    = accum (\(T_ap f g) a -> T_ap (f a) (g a))
            (\(T_ap f g)   -> f g)
            (T_ap f0 g0 :: T_ap a (p -> q) p n)
  {-# INLINE pure  #-}
  {-# INLINE (<*>) #-}

-- | Reader
instance ArityPeano n => Monad (Fun n a) where
  return  = pure
  f >>= g = shuffleFun g <*> f
  {-# INLINE return #-}
  {-# INLINE (>>=)  #-}

newtype T_Flip a b n = T_Flip (Fun n a b)
data T_ap a b c n = T_ap (Fn n a b) (Fn n a c)



----------------------------------------------------------------
-- Generic operations of N-ary functions
----------------------------------------------------------------

-- | Synonym for writing constrains using type level naturals.
type Arity n = ArityPeano (Peano n)

-- | Type class for defining and applying /n/-ary functions.
class ArityPeano n where
  -- | Left fold over /n/ elements exposed as n-ary function. These
  --   elements are supplied as arguments to the function.
  accum :: (forall k. t ('S k) -> a -> t k) -- ^ Fold function
        -> (t 'Z -> b)                      -- ^ Extract result of fold
        -> t n                              -- ^ Initial value
        -> Fun n a b                        -- ^ Reduction function

  -- | Apply all parameters to the function.
  applyFun :: (forall k. t ('S k) -> (a, t k))
              -- ^ Get value to apply to function
           -> t n
              -- ^ Initial value
           -> (ContVec n a, t 'Z)

  -- | Apply all parameters to the function using monadic
  --   actions. Note that for identity monad it's same as
  --   applyFun. Ignoring newtypes:
  --
  -- > forall b. Fn n a b -> b  ~ ContVec n a
  applyFunM :: Applicative f
            => (forall k. t ('S k) -> (f a, t k)) -- ^ Get value to apply to function
            -> t n                                -- ^ Initial value
            -> (f (ContVec n a), t 'Z)

  -- | Conver peano number to int
  peanoToInt :: Proxy# n -> Int

  -- | Reverse order of parameters. It's implemented directly in type
  --   class since expressing it in terms of @accum@ will require
  --   putting Arity constraint on step funcion
  reverseF :: Fun n a b -> Fun n a b

  -- | Worker function for 'gunfold'
  gunfoldF :: (Data a)
           => (forall b x. Data b => c (b -> x) -> c x)
           -> T_gunfold c r a n -> c r

newtype T_gunfold c r a n = T_gunfold (c (Fn n a r))


-- | Apply all parameters to the function.
apply :: ArityPeano n
      => (forall k. t ('S k) -> (a, t k)) -- ^ Get value to apply to function
      -> t n                              -- ^ Initial value
      -> ContVec n a                      -- ^ N-ary function
{-# INLINE apply #-}
apply step z = fst (applyFun step z)

-- | Apply all parameters to the function using applicative actions.
applyM :: (Applicative f, ArityPeano n)
       => (forall k. t ('S k) -> (f a, t k)) -- ^ Get value to apply to function
       -> t n                                -- ^ Initial value
       -> f (ContVec n a)
{-# INLINE applyM #-}
applyM f t = fst $ applyFunM f t


-- | Type class for indexing of vector of length @n@ with statically
--   known index @k@
class Index (k :: PeanoNum) (n :: PeanoNum) where
  getF  :: Proxy# k -> Fun n a a
  putF  :: Proxy# k -> a -> Fun n a r -> Fun n a r
  lensF :: Functor f => Proxy# k -> (a -> f a) -> Fun n a r -> Fun n a (f r)



instance ArityPeano 'Z where
  accum      _ g t = Fun $ g t
  applyFun   _ t   = (ContVec unFun, t)
  applyFunM  _ t   = (pure (ContVec unFun), t)
  peanoToInt _    = 0
  {-# INLINE accum      #-}
  {-# INLINE applyFun   #-}
  {-# INLINE applyFunM  #-}
  {-# INLINE peanoToInt #-}
  reverseF = id
  gunfoldF _ (T_gunfold c) = c
  {-# INLINE reverseF    #-}
  {-# INLINE gunfoldF    #-}

instance ArityPeano n => ArityPeano ('S n) where
  accum     f g t = Fun $ \a -> unFun $ accum f g (f t a)
  applyFun  f t   = let (a,t') = f t
                        (v,tZ) = applyFun f t'
                    in  (consPeano a v, tZ)
  applyFunM f t   = let (a,t')   = f t
                        (vec,t0) = applyFunM f t'
                    in  (consPeano <$> a <*> vec, t0)
  peanoToInt _ = 1 + peanoToInt (proxy# @n)
  {-# INLINE accum      #-}
  {-# INLINE applyFun   #-}
  {-# INLINE applyFunM  #-}
  {-# INLINE peanoToInt #-}
  reverseF f   = Fun $ \a -> unFun (reverseF $ apLast f a)
  gunfoldF f c = gunfoldF f (apGunfold f c)
  {-# INLINE reverseF    #-}
  {-# INLINE gunfoldF    #-}

instance ArityPeano n => Index 'Z ('S n) where
  getF  _       = uncurryFirst pure
  putF  _ a f   = Fun $ \_ -> unFun f a
  lensF _ f fun = Fun $ \a -> unFun $
    (\g -> g <$> f a) <$> shuffleFun (curryFirst fun)
  {-# INLINE getF  #-}
  {-# INLINE putF  #-}
  {-# INLINE lensF #-}

instance Index k n => Index (S k) (S n) where
  getF  _       = uncurryFirst $ \_ -> getF (proxy# @k)
  putF  _ a     = withFun (putF  (proxy# @k) a)
  lensF _ f fun = withFun (lensF (proxy# @k) f) fun
  {-# INLINE getF  #-}
  {-# INLINE putF  #-}
  {-# INLINE lensF #-}



apGunfold :: Data a
          => (forall b x. Data b => c (b -> x) -> c x)
          -> T_gunfold c r a ('S n)
          -> T_gunfold c r a n
apGunfold f (T_gunfold c) = T_gunfold $ f c
{-# INLINE apGunfold #-}



----------------------------------------------------------------
-- Combinators
----------------------------------------------------------------

-- | Prepend ignored parameter to function
constFun :: Fun n a b -> Fun ('S n) a b
constFun (Fun f) = Fun $ \_ -> f
{-# INLINE constFun #-}

-- | Curry first parameter of n-ary function
curryFirst :: Fun ('S n) a b -> a -> Fun n a b
curryFirst = coerce
{-# INLINE curryFirst #-}

-- | Uncurry first parameter of n-ary function
uncurryFirst :: (a -> Fun n a b) -> Fun ('S n) a b
uncurryFirst = coerce
{-# INLINE uncurryFirst #-}

-- | Curry last parameter of n-ary function
curryLast :: ArityPeano n => Fun ('S n) a b -> Fun n a (a -> b)
{-# INLINE curryLast #-}
-- NOTE: This function is essentially rearrangement of newtypes. Since
--       Fn is closed type family it couldn't be extended and it's
--       quite straightforward to show that both types have same
--       representation. Unfortunately GHC cannot infer it so we have
--       to unsafe-coerce it.
curryLast = unsafeCoerce


-- | Curry /n/ first parameters of n-ary function
curryMany :: forall n k a b. ArityPeano n
          => Fun (Add n k) a b -> Fun n a (Fun k a b)
{-# INLINE curryMany #-}
-- NOTE: It's same as curryLast
curryMany = unsafeCoerce


-- | Apply last parameter to function. Unlike 'apFun' we need to
--   traverse all parameters but last hence 'Arity' constraint.
apLast :: ArityPeano n => Fun ('S n) a b -> a -> Fun n a b
apLast f x = fmap ($ x) $ curryLast f
{-# INLINE apLast #-}

-- | Recursive step for the function
withFun :: (Fun n a b -> Fun n a c) -> Fun ('S n) a b -> Fun ('S n) a c
withFun f fun = Fun $ \a -> unFun $ f $ curryFirst fun a
{-# INLINE withFun #-}

-- | Move function parameter to the result of N-ary function.
shuffleFun :: ArityPeano n
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

-- | Size of vector expressed as Peano natural.
type family Dim (v :: Type -> Type) :: PeanoNum

-- | Type class for vectors with fixed length. Instance should provide
--   two functions: one to create vector from @N@ elements and another
--   for vector deconstruction. They must obey following law:
--
--   > inspect v construct = v
--
--   For example instance for 2D vectors could be written as:
--
--   > data V2 a = V2 a a
--   >
--   > type instance V2 = 2
--   > instance Vector V2 a where
--   >   construct                = Fun V2
--   >   inspect (V2 a b) (Fun f) = f a b
class ArityPeano (Dim v) => Vector v a where
  -- | N-ary function for creation of vectors. It takes @N@ elements
  --   of array as parameters and return vector.
  construct :: Fun (Dim v) a (v a)
  -- | Deconstruction of vector. It takes N-ary function as parameters
  --   and applies vector's elements to it.
  inspect   :: v a -> Fun (Dim v) a b -> b
  -- | Optional more efficient implementation of indexing. Shouldn't
  --   be used directly, use 'Data.Vector.Fixed.!' instead.
  basicIndex :: v a -> Int -> a
  basicIndex v i = index i (cvec v)
  {-# INLINE basicIndex #-}

-- | Length of vector. Function doesn't evaluate its argument.
length :: forall v a. ArityPeano (Dim v) => v a -> Int
{-# INLINE length #-}
length _ = peanoToInt (proxy# @(Dim v))


----------------------------------------------------------------
-- Cont. vectors and their instances
----------------------------------------------------------------

-- | Vector represented as continuation. Alternative wording: it's
--   Church encoded N-element vector.
newtype ContVec n a = ContVec (forall r. Fun n a r -> r)

type instance Dim (ContVec n) = n

-- | Cons values to the @ContVec@.
consPeano :: a -> ContVec n a -> ContVec ('S n) a
consPeano a (ContVec cont) = ContVec $ \f -> cont $ curryFirst f a
{-# INLINE consPeano #-}

instance ArityPeano n => Vector (ContVec n) a where
  construct = accum
    (\(T_mkN f) a -> T_mkN (f . consPeano a))
    (\(T_mkN f)   -> f (ContVec unFun))
    (T_mkN id)
  inspect (ContVec c) f = c f
  {-# INLINE construct #-}
  {-# INLINE inspect   #-}

newtype T_mkN n_tot a n = T_mkN (ContVec n a -> ContVec n_tot a)



instance (Eq a, ArityPeano n) => Eq (ContVec n a) where
  a == b = and $ zipWith (==) a b
  {-# INLINE (==) #-}

instance (Ord a, ArityPeano n) => Ord (ContVec n a) where
  compare a b = foldl mappend mempty $ zipWith compare a b
  {-# INLINE compare #-}

instance (ArityPeano n, Monoid a) => Monoid (ContVec n a) where
  mempty = replicate mempty
  {-# INLINE mempty  #-}

instance (ArityPeano n, Semigroup a) => Semigroup (ContVec n a) where
  (<>) = zipWith (<>)
  {-# INLINE (<>) #-}


instance (ArityPeano n) => Functor (ContVec n) where
  fmap = map
  {-# INLINE fmap #-}

instance (ArityPeano n) => Applicative (ContVec n) where
  pure  = replicate
  (<*>) = zipWith ($)
  {-# INLINE pure  #-}
  {-# INLINE (<*>) #-}

instance (ArityPeano n) => F.Foldable (ContVec n) where
  foldMap' f = foldl' (\ acc a -> acc <> f a) mempty
  foldr      = foldr
  foldl      = foldl
  foldl'     = foldl'
  toList     = toList
  sum        = sum
  product    = foldl' (*) 0
  {-# INLINE foldMap' #-}
  {-# INLINE foldr    #-}
  {-# INLINE foldl    #-}
  {-# INLINE foldl'   #-}
  {-# INLINE toList   #-}
  {-# INLINE sum      #-}
  {-# INLINE product  #-}
-- GHC<9.2 fails to compile this
#if MIN_VERSION_base(4,16,0)
  length = length
  {-# INLINE length #-}
#endif

instance (ArityPeano n) => T.Traversable (ContVec n) where
  sequence  = sequence
  sequenceA = sequence
  traverse  = mapM
  mapM      = mapM
  {-# INLINE sequence  #-}
  {-# INLINE sequenceA #-}
  {-# INLINE mapM      #-}
  {-# INLINE traverse  #-}



----------------------------------------------------------------
-- Construction
----------------------------------------------------------------

-- | Convert regular vector to continuation based one.
cvec :: (Vector v a) => v a -> ContVec (Dim v) a
cvec v = ContVec (inspect v)
{-# INLINE[0] cvec #-}

-- | Create empty vector.
empty :: ContVec 'Z a
{-# INLINE empty #-}
empty = ContVec (\(Fun r) -> r)


-- | Convert list to continuation-based vector. Will throw error if
--   list is shorter than resulting vector.
fromList :: ArityPeano n => [a] -> ContVec n a
{-# INLINE fromList #-}
fromList xs =
  apply step (Const xs)
  where
    step (Const []    ) = error "Data.Vector.Fixed.Cont.fromList: too few elements"
    step (Const (a:as)) = (a, Const as)

-- | Same as 'fromList' bu throws error is list doesn't have same
--   length as vector.
fromList' :: forall n a. ArityPeano n => [a] -> ContVec n a
{-# INLINE fromList' #-}
fromList' xs =
  let step (Const []    ) = error "Data.Vector.Fixed.Cont.fromList': too few elements"
      step (Const (a:as)) = (a, Const as)
  in case applyFun step (Const xs :: Const [a] n) of
    (v,Const []) -> v
    _            -> error "Data.Vector.Fixed.Cont.fromList': too many elements"


-- | Convert list to continuation-based vector. Will fail with
--   'Nothing' if list doesn't have right length.
fromListM :: forall n a. ArityPeano n => [a] -> Maybe (ContVec n a)
{-# INLINE fromListM #-}
fromListM xs = case applyFunM step (Const xs :: Const [a] n) of
  (Just v, Const []) -> Just v
  _                  -> Nothing
  where
    step (Const []    ) = (Nothing, Const [])
    step (Const (a:as)) = (Just a , Const as)


-- | Convert vector to the list
toList :: (ArityPeano n) => ContVec n a -> [a]
toList = foldr (:) []
{-# INLINE toList #-}


-- | Execute monadic action for every element of vector. Synonym for 'pure'.
replicate :: (ArityPeano n) => a -> ContVec n a
{-# INLINE replicate #-}
replicate a = apply (\Proxy -> (a, Proxy)) Proxy

-- | Execute monadic action for every element of vector.
replicateM :: (ArityPeano n, Applicative f) => f a -> f (ContVec n a)
{-# INLINE replicateM #-}
replicateM act
  = applyM (\Proxy -> (act, Proxy)) Proxy


-- | Generate vector from function which maps element's index to its value.
generate :: (ArityPeano n) => (Int -> a) -> ContVec n a
{-# INLINE generate #-}
generate f =
  apply (\(Const n) -> (f n, Const (n + 1))) (Const 0)

-- | Generate vector from monadic function which maps element's index
--   to its value.
generateM :: (Applicative f, ArityPeano n) => (Int -> f a) -> f (ContVec n a)
{-# INLINE generateM #-}
generateM f =
  applyM (\(Const n) -> (f n, Const (n + 1))) (Const 0)


-- | Unfold vector.
unfoldr :: ArityPeano n => (b -> (a,b)) -> b -> ContVec n a
{-# INLINE unfoldr #-}
unfoldr f b0 =
  apply (\(Const b) -> let (a,b') = f b in (a, Const b'))
        (Const b0)

-- | Unit vector along Nth axis.
basis :: (Num a, ArityPeano n) => Int -> ContVec n a
{-# INLINE basis #-}
basis n0 =
  apply (\(Const n) -> (if n == 0 then 1 else 0, Const (n - 1)))
        (Const n0)



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

mk6 :: a -> a -> a -> a -> a -> a -> ContVec N6 a
mk6 a1 a2 a3 a4 a5 a6 = ContVec $ \(Fun f) -> f a1 a2 a3 a4 a5 a6
{-# INLINE mk6 #-}

mk7 :: a -> a -> a -> a -> a -> a -> a -> ContVec N7 a
mk7 a1 a2 a3 a4 a5 a6 a7 = ContVec $ \(Fun f) -> f a1 a2 a3 a4 a5 a6 a7
{-# INLINE mk7 #-}

mk8 :: a -> a -> a -> a -> a -> a -> a -> a -> ContVec N8 a
mk8 a1 a2 a3 a4 a5 a6 a7 a8 = ContVec $ \(Fun f) -> f a1 a2 a3 a4 a5 a6 a7 a8
{-# INLINE mk8 #-}


----------------------------------------------------------------
-- Transforming vectors
----------------------------------------------------------------

-- | Map over vector. Synonym for 'fmap'
map :: (ArityPeano n) => (a -> b) -> ContVec n a -> ContVec n b
{-# INLINE map #-}
map f (ContVec contA) = ContVec $
  contA . mapF f

-- | Apply function to every element of the vector and its index.
imap :: (ArityPeano n) => (Int -> a -> b) -> ContVec n a -> ContVec n b
{-# INLINE imap #-}
imap f (ContVec contA) = ContVec $
  contA . imapF f

-- | Effectful map over vector.
mapM :: (ArityPeano n, Applicative f) => (a -> f b) -> ContVec n a -> f (ContVec n b)
{-# INLINE mapM #-}
mapM f v
 = inspect v
 $ mapMF f construct

-- | Apply monadic function to every element of the vector and its index.
imapM :: (ArityPeano n, Applicative f)
      => (Int -> a -> f b) -> ContVec n a -> f (ContVec n b)
{-# INLINE imapM #-}
imapM f v
  = inspect v
  $ imapMF f construct

-- | Apply monadic action to each element of vector and ignore result.
mapM_ :: (ArityPeano n, Applicative f) => (a -> f b) -> ContVec n a -> f ()
{-# INLINE mapM_ #-}
mapM_ f = foldl (\m a -> m *> f a *> pure ()) (pure ())

-- | Apply monadic action to each element of vector and its index and
--   ignore result.
imapM_ :: (ArityPeano n, Applicative f) => (Int -> a -> f b) -> ContVec n a -> f ()
{-# INLINE imapM_ #-}
imapM_ f = ifoldl (\m i a -> m *> f i a *> pure ()) (pure ())



mapMF :: (ArityPeano n, Applicative f)
      => (a -> f b) -> Fun n b r -> Fun n a (f r)
{-# INLINE mapMF #-}
mapMF f (Fun funB) =
  accum (\(T_mapM m) a -> T_mapM (($) <$> m <*> f a))
        (\(T_mapM m) -> m)
        (T_mapM (pure funB))

imapMF :: (ArityPeano n, Applicative f)
       => (Int -> a -> f b) -> Fun n b r -> Fun n a (f r)
{-# INLINE imapMF #-}
imapMF f (Fun funB) =
  accum (\(T_imapM i m) a -> T_imapM (i+1) $ ($) <$> m <*> f i a)
        (\(T_imapM _ m) -> m)
        (T_imapM 0 (pure funB))

newtype T_mapM  a m r n = T_mapM      (m (Fn n a r))
data    T_imapM a m r n = T_imapM Int (m (Fn n a r))


mapF :: ArityPeano n
     => (a -> b) -> Fun n b r -> Fun n a r
{-# INLINE mapF #-}
mapF f (Fun funB) =
  accum (\(T_map g) b -> T_map (g (f b)))
        (\(T_map r)   -> r)
        (  T_map funB)

imapF :: ArityPeano n
      => (Int -> a -> b) -> Fun n b r -> Fun n a r
{-# INLINE imapF #-}
imapF f (Fun funB) =
  accum (\(T_imap i g) b -> T_imap (i+1) (g (f i b)))
        (\(T_imap _ r)   -> r)
        (  T_imap 0 funB)

newtype T_map  a r n = T_map      (Fn n a r)
data    T_imap a r n = T_imap Int (Fn n a r)

-- | Left scan over vector
scanl :: (ArityPeano n) => (b -> a -> b) -> b -> ContVec n a -> ContVec ('S n) b
{-# INLINE scanl #-}
scanl f b0 (ContVec cont) = ContVec $
  cont . scanlF f b0

-- | Left scan over vector
scanl1 :: (ArityPeano n) => (a -> a -> a) -> ContVec n a -> ContVec n a
{-# INLINE scanl1 #-}
scanl1 f (ContVec cont) = ContVec $
  cont . scanl1F f

scanlF :: forall n a b r. (ArityPeano n) => (b -> a -> b) -> b -> Fun ('S n) b r -> Fun n a r
scanlF f b0 (Fun fun0)
  = accum step fini start
  where
    step  :: forall k. T_scanl r b ('S k) -> a -> T_scanl r b k
    step (T_scanl b fn) a = let b' = f b a in T_scanl b' (fn b')
    fini (T_scanl _ r) = r
    start = T_scanl b0 (fun0 b0)  :: T_scanl r b n

scanl1F :: forall n a r. (ArityPeano n) => (a -> a -> a) -> Fun n a r -> Fun n a r
scanl1F f (Fun fun0) = accum step fini start
  where
    step  :: forall k. T_scanl1 r a ('S k) -> a -> T_scanl1 r a k
    step (T_scanl1 Nothing  fn) a = T_scanl1 (Just a) (fn a)
    step (T_scanl1 (Just x) fn) a = let a' = f x a in T_scanl1 (Just a') (fn a')
    fini (T_scanl1 _ r) = r
    start = T_scanl1 Nothing fun0 :: T_scanl1 r a n

data T_scanl  r a n = T_scanl a (Fn n a r)
data T_scanl1 r a n = T_scanl1 (Maybe a) (Fn n a r)


-- | Evaluate every action in the vector from left to right.
sequence :: (ArityPeano n, Applicative f) => ContVec n (f a) -> f (ContVec n a)
sequence = mapM id
{-# INLINE sequence #-}

-- | Evaluate every action in the vector from left to right and ignore result.
sequence_ :: (ArityPeano n, Applicative f) => ContVec n (f a) -> f ()
sequence_ = mapM_ id
{-# INLINE sequence_ #-}

-- | The dual of sequenceA
distribute :: (Functor f, ArityPeano n) => f (ContVec n a) -> ContVec n (f a)
{-# INLINE distribute #-}
distribute f0
  = apply step start
  where
    -- It's not possible to use ContVec as accumulator type since `head'
    -- require Arity constraint on `k'. So we use plain lists
    step (Const f) = ( fmap (\(x:_) -> x) f
                     , Const $ fmap (\(_:x) -> x) f)
    start = Const (fmap toList f0)

collect :: (Functor f, ArityPeano n) => (a -> ContVec n b) -> f a -> ContVec n (f b)
collect f = distribute . fmap f
{-# INLINE collect #-}

-- | /O(1)/ Tail of vector.
tail :: ContVec (S n) a -> ContVec n a
tail (ContVec cont) = ContVec $ \f -> cont $ constFun f
{-# INLINE tail #-}

-- | /O(1)/ Prepend element to vector
cons :: a -> ContVec n a -> ContVec ('S n) a
cons a (ContVec cont) = ContVec $ \f -> cont $ curryFirst f a
{-# INLINE cons #-}

-- | Prepend single element vector to another vector.
consV :: ArityPeano n => ContVec N1 a -> ContVec n a -> ContVec ('S n) a
{-# INLINE consV #-}
consV (ContVec cont1) (ContVec cont)
  = ContVec $ \f -> cont $ curryFirst f $ cont1 $ Fun id

-- | /O(1)/ Append element to vector
snoc :: ArityPeano n => a -> ContVec n a -> ContVec ('S n) a
snoc a (ContVec cont) = ContVec $ \f -> cont $ apLast f a
{-# INLINE snoc #-}


-- | Concatenate vector
concat :: ( ArityPeano n
          , ArityPeano k
          , ArityPeano (n `Add` k)
          )
       => ContVec n a -> ContVec k a -> ContVec (Add n k) a
{-# INLINE concat #-}
concat v u = inspect u
           $ inspect v
           $ curryMany construct

-- | Reverse order of elements in the vector
reverse :: ArityPeano n => ContVec n a -> ContVec n a
reverse (ContVec cont) = ContVec $ cont . reverseF
{-# INLINE reverse #-}

-- | Zip two vector together using function.
zipWith :: (ArityPeano n) => (a -> b -> c)
        -> ContVec n a -> ContVec n b -> ContVec n c
{-# INLINE zipWith #-}
zipWith f vecA vecB = ContVec $ \funC ->
    inspect vecB
  $ inspect vecA
  $ zipWithF f funC

-- | Zip three vectors together
zipWith3 :: (ArityPeano n) => (a -> b -> c -> d)
         -> ContVec n a -> ContVec n b -> ContVec n c -> ContVec n d
{-# INLINE zipWith3 #-}
zipWith3 f v1 v2 v3 = zipWith (\a (b, c) -> f a b c) v1 (zipWith (,) v2 v3)

-- | Zip two vector together using function which takes element index
--   as well.
izipWith :: (ArityPeano n) => (Int -> a -> b -> c)
         -> ContVec n a -> ContVec n b -> ContVec n c
{-# INLINE izipWith #-}
izipWith f vecA vecB = ContVec $ \funC ->
    inspect vecB
  $ inspect vecA
  $ izipWithF f funC

-- | Zip three vectors together
izipWith3 :: (ArityPeano n) => (Int -> a -> b -> c -> d)
          -> ContVec n a -> ContVec n b -> ContVec n c -> ContVec n d
{-# INLINE izipWith3 #-}
izipWith3 f v1 v2 v3 = izipWith (\i a (b, c) -> f i a b c) v1 (zipWith (,) v2 v3)

-- | Zip two vector together using monadic function.
zipWithM :: (ArityPeano n, Applicative f) => (a -> b -> f c)
         -> ContVec n a -> ContVec n b -> f (ContVec n c)
{-# INLINE zipWithM #-}
zipWithM f v w = sequence $ zipWith f v w

zipWithM_ :: (ArityPeano n, Applicative f)
          => (a -> b -> f c) -> ContVec n a -> ContVec n b -> f ()
{-# INLINE zipWithM_ #-}
zipWithM_ f xs ys = sequence_ (zipWith f xs ys)

-- | Zip two vector together using monadic function which takes element
--   index as well..
izipWithM :: (ArityPeano n, Applicative f) => (Int -> a -> b -> f c)
          -> ContVec n a -> ContVec n b -> f (ContVec n c)
{-# INLINE izipWithM #-}
izipWithM f v w = sequence $ izipWith f v w

izipWithM_ :: (ArityPeano n, Applicative f)
           => (Int -> a -> b -> f c) -> ContVec n a -> ContVec n b -> f ()
{-# INLINE izipWithM_ #-}
izipWithM_ f xs ys = sequence_ (izipWith f xs ys)

-- NOTE: [zipWith]
-- ~~~~~~~~~~~~~~~
--
-- It turns out it's very difficult to implement zipWith using
-- accum/apply. Key problem is we need to implement:
--
-- > zipF :: Fun n (a,b) r → Fun n a (Fun b r)
--
-- Induction step would be implementing
--
-- > ((a,b) → Fun n (a,b) r) → (a → Fun n a (b → Fun b r))
--
-- in terms of zipF above. It will give us `Fun n a (Fun b r)` but
-- we'll need to move parameter `b` _inside_ `Fun n a`. This requires
-- `ArityPeano` constraint while accum's parameter has note. Even
-- worse this implementation has quadratic complexity.
--
-- It's possible to make zipF method of ArityPeano but quadratic
-- complexity won't go away and starts cause slowdown even for modest
-- values of `n`: 5-6. For n above 10 compilation starts to fail with
-- "simplifier ticks exhausted error".
--
-- It turns out easiest way is materialize list and then deconstruct.
-- GHC is able to eliminate it and it's very hard to beat this approach

zipWithF :: (ArityPeano n)
          => (a -> b -> c) -> Fun n c r -> Fun n a (Fun n b r)
{-# INLINE zipWithF #-}
zipWithF f (Fun g0)
  = makeList
  $ \v -> accum (\(T_zip (a:as) g) b -> T_zip as (g $ f a b))
                (\(T_zip _      x)   -> x)
                (T_zip v g0)

izipWithF :: (ArityPeano n)
          => (Int -> a -> b -> c) -> Fun n c r -> Fun n a (Fun n b r)
{-# INLINE izipWithF #-}
izipWithF f (Fun g0)
  = makeList
  $ \v -> accum (\(T_izip i (a:as) g) b -> T_izip (i+1) as (g $ f i a b))
                (\(T_izip _ _      x)   -> x)
                (T_izip 0 v g0)

makeList :: ArityPeano n => ([a] -> b) -> Fun n a b
{-# INLINE makeList #-}
makeList cont = accum
    (\(Const xs) x -> Const (xs . (x:)))
    (\(Const xs) -> cont (xs []))
    (Const id)

data T_izip a c r n = T_izip Int [a] (Fn n c r)
data T_zip  a c r n = T_zip      [a] (Fn n c r)



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
vector :: (Vector v a) => ContVec (Dim v) a -> v a
vector = runContVec construct
{-# INLINE[1] vector #-}

-- | Finalizer function for getting head of the vector.
head :: (ArityPeano n, n ~ 'S k) => ContVec n a -> a
{-# INLINE head #-}
head
  = runContVec
  $ accum (\(Const m) a -> Const $ case m of { Nothing -> Just a; x -> x })
          (\(Const (Just x)) -> x)
          (Const Nothing)


-- | /O(n)/ Get value at specified index.
index :: ArityPeano n => Int -> ContVec n a -> a
{-# INLINE index #-}
index n
  | n < 0     = error "Data.Vector.Fixed.Cont.index: index out of range"
  | otherwise = runContVec $ accum
     (\(Const x) a -> Const $ case x of
                        Left  0 -> Right a
                        Left  i -> Left (i - 1)
                        r       -> r
     )
     (\(Const x) -> case x of
                      Left  _ -> error "Data.Vector.Fixed.index: index out of range"
                      Right a -> a
     )
     (Const (Left n))


-- | Twan van Laarhoven lens for continuation based vector
element :: (ArityPeano n, Functor f)
        => Int -> (a -> f a) -> ContVec n a -> f (ContVec n a)
{-# INLINE element #-}
element i f v = inspect v
              $ elementF i f construct

-- | Helper for implementation of Twan van Laarhoven lens.
elementF :: forall a n f r. (ArityPeano n, Functor f)
         => Int -> (a -> f a) -> Fun n a r -> Fun n a (f r)
{-# INLINE elementF #-}
elementF n f (Fun fun0) = accum step fini start
  where
    step :: forall k. T_lens f a r ('S k) -> a -> T_lens f a r k
    step (T_lens (Left (0,fun))) a = T_lens $ Right $ fmap fun $ f a
    step (T_lens (Left (i,fun))) a = T_lens $ Left (i-1, fun a)
    step (T_lens (Right fun))    a = T_lens $ Right $ fmap ($ a) fun
    --
    fini :: T_lens f a r 'Z -> f r
    fini (T_lens (Left  _)) = error "Data.Vector.Fixed.lensF: Index out of range"
    fini (T_lens (Right r)) = r
    --
    start :: T_lens f a r n
    start = T_lens $ Left (n,fun0)

data T_lens f a r n = T_lens (Either (Int,(Fn n a r)) (f (Fn n a r)))



-- | Left fold over continuation vector.
foldl :: ArityPeano n => (b -> a -> b) -> b -> ContVec n a -> b
{-# INLINE foldl #-}
foldl f b0 v
  = inspect v
  $ accum (\(T_foldl b) a -> T_foldl (f b a))
          (\(T_foldl b)   -> b)
          (T_foldl b0)

-- | Strict left fold over continuation vector.
foldl' :: ArityPeano n => (b -> a -> b) -> b -> ContVec n a -> b
{-# INLINE foldl' #-}
foldl' f b0 v
  = inspect v
  $ accum (\(T_foldl !b) a -> T_foldl (f b a))
          (\(T_foldl b)    -> b)
          (T_foldl b0)

-- | Left fold over continuation vector.
ifoldl :: ArityPeano n => (b -> Int -> a -> b) -> b -> ContVec n a -> b
{-# INLINE ifoldl #-}
ifoldl f b v
  = inspect v
  $ accum (\(T_ifoldl i r) a -> T_ifoldl (i+1) (f r i a))
          (\(T_ifoldl _ r)   -> r)
          (T_ifoldl 0 b)

-- | Strict left fold over continuation vector.
ifoldl' :: ArityPeano n => (b -> Int -> a -> b) -> b -> ContVec n a -> b
{-# INLINE ifoldl' #-}
ifoldl' f b v
  = inspect v
  $ accum (\(T_ifoldl i !r) a -> T_ifoldl (i+1) (f r i a))
          (\(T_ifoldl _ r)    -> r)
          (T_ifoldl 0 b)

-- | Monadic left fold over continuation vector.
foldM :: (ArityPeano n, Monad m)
      => (b -> a -> m b) -> b -> ContVec n a -> m b
{-# INLINE foldM #-}
foldM f x
  = foldl (\m a -> do{ b <- m; f b a}) (return x)

-- | Monadic left fold over continuation vector.
ifoldM :: (ArityPeano n, Monad m)
       => (b -> Int -> a -> m b) -> b -> ContVec n a -> m b
{-# INLINE ifoldM #-}
ifoldM f x
  = ifoldl (\m i a -> do{ b <- m; f b i a}) (return x)

newtype T_foldl  b n = T_foldl       b
data    T_ifoldl b n = T_ifoldl !Int b

-- | Left fold without base case. It's total because it requires vector to be nonempty
foldl1 :: (ArityPeano n, n ~ 'S k) => (a -> a -> a) -> ContVec n a -> a
{-# INLINE foldl1 #-}
-- Implementation of foldl1 is quite ugly. It could be expressed in
-- terms of foldlF (worker function for foldl)
--
-- > foldl1F f = uncurryFirst $ \a -> foldlF f a
--
-- But it require constraint `ArityPeano n` whereas `Vector v a` gives
-- `ArityPeano (S n)`. Latter imply former but GHC cannot infer it.
foldl1 f
  = runContVec
  $ accum (\(Const r       ) a -> Const $ Just $ maybe a (flip f a) r)
          (\(Const (Just x))   -> x)
          (Const Nothing)

-- | Right fold over continuation vector
foldr :: ArityPeano n => (a -> b -> b) -> b -> ContVec n a -> b
{-# INLINE foldr #-}
foldr = ifoldr . const

-- | Right fold over continuation vector
ifoldr :: ArityPeano n => (Int -> a -> b -> b) -> b -> ContVec n a -> b
{-# INLINE ifoldr #-}
ifoldr f z
  = runContVec
  $ accum (\(T_ifoldr i g) a -> T_ifoldr (i+1) (g . f i a))
          (\(T_ifoldr _ g)   -> g z)
          (T_ifoldr 0 id)

data T_ifoldr b n = T_ifoldr Int (b -> b)

-- | Sum all elements in the vector.
sum :: (Num a, ArityPeano n) => ContVec n a -> a
sum = foldl' (+) 0
{-# INLINE sum #-}

-- | Minimal element of vector.
minimum :: (Ord a, ArityPeano n, n ~ 'S k) => ContVec n a -> a
minimum = foldl1 min
{-# INLINE minimum #-}

-- | Maximal element of vector.
maximum :: (Ord a, ArityPeano n, n ~ 'S k) => ContVec n a -> a
maximum = foldl1 max
{-# INLINE maximum #-}

-- | Conjunction of elements of a vector.
and :: ArityPeano n => ContVec n Bool -> Bool
and = foldr (&&) True
{-# INLINE and #-}

-- | Disjunction of all elements of a vector.
or :: ArityPeano n => ContVec n Bool -> Bool
or = foldr (||) False
{-# INLINE or #-}

-- | Determines whether all elements of vector satisfy predicate.
all :: ArityPeano n => (a -> Bool) -> ContVec n a -> Bool
all f = foldr (\x b -> f x && b) True
{-# INLINE all #-}

-- | Determines whether any of element of vector satisfy predicate.
any :: ArityPeano n => (a -> Bool) -> ContVec n a -> Bool
any f = foldr (\x b -> f x || b) False
{-# INLINE any #-}

-- | The 'find' function takes a predicate and a vector and returns
--   the leftmost element of the vector matching the predicate,
--   or 'Nothing' if there is no such element.
find :: ArityPeano n => (a -> Bool) -> ContVec n a -> Maybe a
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


gfoldlF :: (ArityPeano n, Data a)
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

instance Vector Complex a where
  construct = Fun (:+)
  inspect (x :+ y) (Fun f) = f x y
  {-# INLINE construct #-}
  {-# INLINE inspect #-}


type instance Dim Identity = N1

instance Vector Identity a where
  construct = Fun Identity
  inspect (Identity x) (Fun f) = f x
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


type instance Dim ((,,,,,,) a b c d e f) = N7

instance (b~a, c~a, d~a, e~a, f~a, g~a) => Vector ((,,,,,,) b c d e f g) a where
  construct = Fun (,,,,,,)
  inspect (a,b,c,d,e,f,g) (Fun fun) = fun a b c d e f g
  {-# INLINE construct #-}
  {-# INLINE inspect #-}

type instance Dim Proxy = Z

instance Vector Proxy a where
  construct = Fun Proxy
  inspect _ = unFun
