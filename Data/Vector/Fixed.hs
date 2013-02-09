{-# LANGUAGE EmptyDataDecls        #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
-- |
-- Generic API for vectors with fixed length.
--
-- For encoding of vector size library uses Peano naturals defined in
-- the library. At come point in the future it would make sense to
-- switch to new GHC type level numerals.
module Data.Vector.Fixed (
    -- * Vector type class
    -- ** Vector size
    Dim
  , Z
  , S
    -- ** Synonyms for small numerals
  , N1
  , N2
  , N3
  , N4
  , N5
  , N6
    -- ** Type class
  , Vector(..)
  , VectorN
  , Arity
  , Fun(..)
  , length
    -- * Generic functions
    -- ** Literal vectors
  , New
  , vec
  , con
  , (|>)
    -- ** Construction
  , replicate
  , replicateM
  , basis
  , generate
  , generateM
    -- ** Element access
  , head
  , tail
  , (!)
    -- ** Comparison
  , eq
    -- ** Map
  , map
  , mapM
  , mapM_
  , imap
  , imapM
  , imapM_
  , sequence
  , sequence_
    -- ** Folding
  , foldl
  , foldr
  , foldl1
  , ifoldl
  , ifoldr
  , foldM
  , ifoldM
    -- *** Special folds
  , sum
  , maximum
  , minimum
  , and
  , or
  , all
  , any
    -- ** Zips
  , zipWith
  , zipWithM
  , izipWith
  , izipWithM
    -- ** Conversion
  , convert
  , toList
  , fromList
    -- * Special types
  , VecList
  ) where

import Data.Vector.Fixed.Internal
import Data.Vector.Fixed.Cont     (VecList)
import qualified Data.Vector.Fixed.Cont as C

import qualified Prelude as P
import Prelude hiding ( replicate,map,zipWith,maximum,minimum,and,or,all,any
                      , foldl,foldr,foldl1,length,sum
                      , head,tail,mapM,mapM_,sequence,sequence_
                      )



----------------------------------------------------------------
-- Generic functions
----------------------------------------------------------------

type N1 = S Z
type N2 = S N1
type N3 = S N2
type N4 = S N3
type N5 = S N4
type N6 = S N5


-- TODO: does not fuse!

-- | Generic function for construction of arbitrary vectors. It
--   represents partially constructed vector where /n/ is number of
--   uninitialized elements, /v/ is type of vector and /a/ element type.
--
--   Uninitialized vector could be obtained from 'con' and vector
--   elements could be added from left to right using '|>' operator.
--   Finally it could be converted to vector using 'vec' function.
--
--   Construction of complex number which could be seen as 2-element vector:
--
--   >>> import Data.Complex
--   >>> vec $ con |> 1 |> 3 :: Complex Double
--   1.0 :+ 3.0
--
newtype New n v a = New (Fn n a (v a))

-- | Convert fully applied constructor to vector
vec :: New Z v a -> v a
{-# INLINE vec #-}
vec (New v) = v

-- | Seed constructor
con :: Vector v a => New (Dim v) v a
{-# INLINE con #-}
con = f2n construct

-- | Apply another element to vector
(|>) :: New (S n) v a -> a -> New n v a
{-# INLINE  (|>) #-}
New f |> a = New (f a)
infixl 1 |>

f2n :: Fun n a (v a) -> New n v a
{-# INLINE f2n #-}
f2n (Fun f) = New f


----------------------------------------------------------------

-- | Replicate value /n/ times.
--
--   Examples:
--
--   >>> import Data.Vector.Fixed.Boxed (Vec2)
--   >>> replicate 1 :: Vec2 Int     -- Two element vector
--   fromList [1,1]
--
--   >>> import Data.Vector.Fixed.Boxed (Vec3)
--   >>> replicate 2 :: Vec3 Double  -- Three element vector
--   fromList [2.0,2.0,2.0]
--
--   >>> import Data.Vector.Fixed.Boxed (Vec)
--   >>> replicate "foo" :: Vec N5 String
--   fromList ["foo","foo","foo","foo","foo"]
--
replicate :: Vector v a => a -> v a
{-# INLINE replicate #-}
replicate
  = C.vector . C.replicate

-- | Execute monadic action for every element of vector.
--
--   Examples:
--
--   >>> import Data.Vector.Fixed.Boxed (Vec2,Vec3)
--   >>> replicateM (Just 3) :: Maybe (Vec3 Int)
--   Just fromList [3,3,3]
--   >>> replicateM (putStrLn "Hi!") :: IO (Vec2 ())
--   Hi!
--   Hi!
--   fromList [(),()]
--
replicateM :: (Vector v a, Monad m) => m a -> m (v a)
{-# INLINE replicateM #-}
replicateM
  = C.vectorM . C.replicateM



----------------------------------------------------------------

-- | Unit vector along Nth axis,
--
--   Examples:
--
--   >>> import Data.Vector.Fixed.Boxed (Vec3)
--   >>> basis 0 :: Vec3 Int
--   fromList [1,0,0]
--   >>> basis 1 :: Vec3 Int
--   fromList [0,1,0]
--   >>> basis 2 :: Vec3 Int
--   fromList [0,0,1]
--
basis :: forall v a. (Vector v a, Num a) => Int -> v a
{-# INLINE basis #-}
basis n = create $ Cont
        $ basisF n

newtype T_basis n = T_basis Int

basisF :: forall n a b. (Num a, Arity n) => Int -> Fun n a b -> b
basisF n0 (Fun f)
  = apply (\(T_basis n) -> ((if n == 0 then 1 else 0) :: a, T_basis (n - 1)))
          (T_basis n0 :: T_basis n)
          f


----------------------------------------------------------------

-- | Generate vector from function which maps element's index to its value.
--
--   Examples:
--
--   >>> import Data.Vector.Fixed.Unboxed (Vec)
--   >>> generate (^2) :: Vec N4 Int
--   fromList [0,1,4,9]
--
generate :: forall v a. (Vector v a) => (Int -> a) -> v a
{-# INLINE generate #-}
generate = C.vector . C.generate

-- | Monadic generation
generateM :: forall m v a. (Monad m, Vector v a) => (Int -> m a) -> m (v a)
{-# INLINE generateM #-}
generateM = C.vectorM . C.generateM



----------------------------------------------------------------

-- | First element of vector.
--
--   Examples:
--
--   >>> import Data.Vector.Fixed.Boxed (Vec3)
--   >>> let x = vec $ con |> 1 |> 2 |> 3 :: Vec3 Int
--   >>> head x
--   1
--
head :: (Vector v a, Dim v ~ S n) => v a -> a
{-# INLINE head #-}
head v = inspectV v
       $ headF

data T_head a n = T_head (Maybe a)

headF :: forall n a. Arity (S n) => Fun (S n) a a
headF = Fun $ accum (\(T_head m) a -> T_head $ case m of { Nothing -> Just a; x -> x })
                    (\(T_head (Just x)) -> x)
                    (T_head Nothing :: T_head a (S n))


----------------------------------------------------------------

-- | Tail of vector.
--
--   Examples:
--
--   >>> import Data.Vector.Fixed.Boxed (Vec2, Vec3)
--   >>> let x = vec $ con |> 1 |> 2 |> 3 :: Vec3 Int
--   >>> tail x :: Vec2 Int
--   fromList [2,3]
--
tail :: (Vector v a, Vector w a, Dim v ~ S (Dim w))
     => v a -> w a
{-# INLINE tail #-}
tail = C.vector . C.tail . C.cvec



----------------------------------------------------------------

-- | /O(n)/ Get vector's element at index i.
(!) :: (Vector v a) => v a -> Int -> a
{-# INLINE (!) #-}
v ! i = inspectV v
      $ elemF i

newtype T_Elem a n = T_Elem (Either Int a)

elemF :: forall n a. Arity n => Int -> Fun n a a
elemF n
  -- This is needed because of possible underflow during subtraction
  | n < 0     = error "Data.Vector.Fixed.!: index out of range"
  | otherwise = Fun $ accum
     (\(T_Elem x) a -> T_Elem $ case x of
                         Left  0 -> Right a
                         Left  i -> Left (i - 1)
                         r       -> r
     )
     (\(T_Elem x) -> case x of
                       Left  _ -> error "Data.Vector.Fixed.!: index out of range"
                       Right a -> a
     )
     ( T_Elem (Left n) :: T_Elem a n)


----------------------------------------------------------------

-- | Left fold over vector
foldl :: Vector v a => (b -> a -> b) -> b -> v a -> b
{-# INLINE foldl #-}
foldl f x = C.runContVec (C.foldl f x)
          . C.cvec

-- | Left fold over vector
foldr :: Vector v a => (a -> b -> b) -> b -> v a -> b
{-# INLINE foldr #-}
foldr f x = C.runContVec (C.foldr f x)
          . C.cvec

-- | Left fold over vector
foldl1 :: (Vector v a, Dim v ~ S n) => (a -> a -> a) -> v a -> a
{-# INLINE foldl1 #-}
foldl1 f = C.runContVec (C.foldl1 f)
         . C.cvec

-- | Left fold over vector
ifoldr :: Vector v a => (Int -> a -> b -> b) -> b -> v a -> b
{-# INLINE ifoldr #-}
ifoldr f x = C.runContVec (C.ifoldr f x)
           . C.cvec

-- | Left fold over vector. Function is applied to each element and
--   its index.
ifoldl :: Vector v a => (b -> Int -> a -> b) -> b -> v a -> b
{-# INLINE ifoldl #-}
ifoldl f z = C.runContVec (C.ifoldl f z)
           . C.cvec

-- | Monadic fold over vector.
foldM :: (Vector v a, Monad m) => (b -> a -> m b) -> b -> v a -> m b
{-# INLINE foldM #-}
foldM f x v = foldl go (return x) v
  where
    go m a = do b <- m
                f b a

-- | Left monadic fold over vector. Function is applied to each element and
--   its index.
ifoldM :: (Vector v a, Monad m) => (b -> Int -> a -> m b) -> b -> v a -> m b
{-# INLINE ifoldM #-}
ifoldM f x v = ifoldl go (return x) v
  where
    go m i a = do { b <- m; f b i a }



----------------------------------------------------------------

-- | Sum all elements in the vector
sum :: (Vector v a, Num a) => v a -> a
sum = C.runContVec C.sum . C.cvec
{-# INLINE sum #-}

-- | Maximum element of vector
--
--   Examples:
--
--   >>> import Data.Vector.Fixed.Boxed (Vec3)
--   >>> let x = vec $ con |> 1 |> 2 |> 3 :: Vec3 Int
--   >>> maximum x
--   3
--
maximum :: (Vector v a, Dim v ~ S n, Ord a) => v a -> a
maximum = C.runContVec C.maximum . C.cvec
{-# INLINE maximum #-}

-- | Minimum element of vector
--
--   Examples:
--
--   >>> import Data.Vector.Fixed.Boxed (Vec3)
--   >>> let x = vec $ con |> 1 |> 2 |> 3 :: Vec3 Int
--   >>> minimum x
--   1
--
minimum :: (Vector v a, Dim v ~ S n, Ord a) => v a -> a
minimum = C.runContVec C.minimum . C.cvec
{-# INLINE minimum #-}

and :: (Vector v Bool) => v Bool -> Bool
and = C.runContVec C.and . C.cvec
{-# INLINE and #-}

or :: (Vector v Bool) => v Bool -> Bool
or = C.runContVec C.or . C.cvec
{-# INLINE or #-}

all :: (Vector v a) => (a -> Bool) -> v a -> Bool
all f = C.runContVec (C.all f) . C.cvec
{-# INLINE all #-}

any :: (Vector v a) => (a -> Bool) -> v a -> Bool
any f = C.runContVec (C.any f) . C.cvec
{-# INLINE any #-}


----------------------------------------------------------------

-- | Test two vectors for equality.
--
--   Examples:
--
--   >>> import Data.Vector.Fixed.Boxed (Vec2)
--   >>> let v0 = basis 0 :: Vec2 Int
--   >>> let v1 = basis 1 :: Vec2 Int
--   >>> v0 `eq` v0
--   True
--   >>> v0 `eq` v1
--   False
--
eq :: (Vector v a, Eq a) => v a -> v a -> Bool
{-# INLINE eq #-}
eq v w = C.runContVec (C.foldl (&&) True)
       $ C.zipWith (==) (C.cvec v) (C.cvec w)


----------------------------------------------------------------

-- | Map over vector
map :: (Vector v a, Vector v b) => (a -> b) -> v a -> v b
{-# INLINE map #-}
map f = C.vector
      . C.map f
      . C.cvec

-- | Evaluate every action in the vector from left to right.
sequence :: (Vector v a, Vector v (m a), Monad m) => v (m a) -> m (v a)
{-# INLINE sequence #-}
sequence = mapM id

-- | Evaluate every action in the vector from left to right and ignore result
sequence_ :: (Vector v (m a), Monad m) => v (m a) -> m ()
{-# INLINE sequence_ #-}
sequence_ = mapM_ id


-- | Monadic map over vector.
mapM :: (Vector v a, Vector v b, Monad m) => (a -> m b) -> v a -> m (v b)
{-# INLINE mapM #-}
mapM f = C.vectorM
       . C.mapM f
       . C.cvec

-- | Apply monadic action to each element of vector and ignore result.
mapM_ :: (Vector v a, Monad m) => (a -> m b) -> v a -> m ()
{-# INLINE mapM_ #-}
mapM_ f = foldl (\m a -> m >> f a >> return ()) (return ())


-- | Apply function to every element of the vector and its index.
imap :: (Vector v a, Vector v b) =>
    (Int -> a -> b) -> v a -> v b
{-# INLINE imap #-}
imap f = C.vector
       . C.imap f
       . C.cvec

-- | Apply monadic function to every element of the vector and its index.
imapM :: (Vector v a, Vector v b, Monad m) =>
    (Int -> a -> m b) -> v a -> m (v b)
{-# INLINE imapM #-}
imapM f = C.vectorM
        . C.imapM f
        . C.cvec

-- | Apply monadic function to every element of the vector and its
--   index and discard result.
imapM_ :: (Vector v a, Monad m) => (Int -> a -> m b) -> v a -> m ()
{-# INLINE imapM_ #-}
imapM_ f = ifoldl (\m i a -> m >> f i a >> return ()) (return ())


----------------------------------------------------------------

-- | Zip two vector together using function.
--
--   Examples:
--
--   >>> import Data.Vector.Fixed.Boxed (Vec3)
--   >>> let b0 = basis 0 :: Vec3 Int
--   >>> let b1 = basis 1 :: Vec3 Int
--   >>> let b2 = basis 2 :: Vec3 Int
--   >>> let vplus x y = zipWith (+) x y
--   >>> vplus b0 b1
--   fromList [1,1,0]
--   >>> vplus b0 b2
--   fromList [1,0,1]
--   >>> vplus b1 b2
--   fromList [0,1,1]
--
zipWith :: (Vector v a, Vector v b, Vector v c)
        => (a -> b -> c) -> v a -> v b -> v c
{-# INLINE zipWith #-}
zipWith f v u = C.vector
              $ C.zipWith f (C.cvec v) (C.cvec u)

-- | Zip two vector together using monadic function.
zipWithM :: (Vector v a, Vector v b, Vector v c, Monad m)
         => (a -> b -> m c) -> v a -> v b -> m (v c)
{-# INLINE zipWithM #-}
zipWithM f v u = C.vectorM
               $ C.zipWithM f (C.cvec v) (C.cvec u)

-- | Zip two vector together using function which takes element index
--   as well.
izipWith :: (Vector v a, Vector v b, Vector v c)
         => (Int -> a -> b -> c) -> v a -> v b -> v c
{-# INLINE izipWith #-}
izipWith f v u = C.vector
               $ C.izipWith f (C.cvec v) (C.cvec u)

-- | Zip two vector together using monadic function which takes element
--   index as well..
izipWithM :: (Vector v a, Vector v b, Vector v c, Monad m)
          => (Int -> a -> b -> m c) -> v a -> v b -> m (v c)
{-# INLINE izipWithM #-}
izipWithM f v u = C.vectorM
                $ C.izipWithM f (C.cvec v) (C.cvec u)


----------------------------------------------------------------

-- | Convert between different vector types
convert :: (Vector v a, Vector w a, Dim v ~ Dim w) => v a -> w a
{-# INLINE convert #-}
convert = C.vector . C.cvec

-- | Convert vector to the list
toList :: (Vector v a) => v a -> [a]
toList = foldr (:) []

-- | Create vector form list. List must have same length as the
--   vector.
fromList :: (Vector v a) => [a] -> v a
{-# INLINE fromList #-}
fromList = C.vector . C.fromList
