{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE Rank2Types            #-}
-- |
-- Implementation of fixed-vectors
module Data.Vector.Fixed.Internal where

import Control.Applicative (Applicative)
import Control.Monad       (liftM)
import Data.Monoid         (Monoid(..))
import qualified Data.Foldable    as T
import qualified Data.Traversable as T


import Data.Vector.Fixed.Cont     (Vector(..),Dim,S,Z,Arity,vector)
import qualified Data.Vector.Fixed.Cont as C
import           Data.Vector.Fixed.Cont   (ContVec,Index)
import qualified Prelude as P
import Prelude hiding ( replicate,map,zipWith,maximum,minimum,and,or,all,any
                      , foldl,foldr,foldl1,length,sum,reverse
                      , head,tail,mapM,mapM_,sequence,sequence_
                      )


----------------------------------------------------------------
-- Constructors
----------------------------------------------------------------

-- | Variadic vector constructor. Resulting vector should be converted
--   from 'ContVec' using 'vector' function.  For example:
--
-- >>> vector $ mkN 'a' 'b' 'c' :: (Char,Char,Char)
-- ('a','b','c')
mkN :: Make (S Z) a r => a -> r
mkN = unGo $ make id
{-# INLINE mkN #-}


-- | Type class for variadic vector constructors.
class Make n a r where
  make :: (ContVec Z a -> ContVec n a) -> r

instance (a'~a, Make (S n) a r) => Make n a' (a -> r) where
  make f a = make (C.cons a . f)
  {-# INLINE make #-}

instance Arity n =>  Make n a (ContVec n a) where
  make f = C.reverse $ f C.empty
  {-# INLINE make #-}

newtype Go r = Go { unGo :: r }

instance Make Z a r => Make Z a (Go r) where
  make f = Go $ make f
  {-# INLINE make #-}

-- | Cons value to continuation based vector.
(<|) :: a -> ContVec n a -> ContVec (S n) a
(<|) = C.cons
{-# INLINE (<|) #-}

infixr 1 <|


mk0 :: (Vector v a, Dim v ~ C.N0) => v a
mk0 = vector $ C.empty
{-# INLINE mk0 #-}

mk1 :: (Vector v a, Dim v ~ C.N1) => a -> v a
mk1 a1 = vector $ C.mk1 a1
{-# INLINE mk1 #-}

mk2 :: (Vector v a, Dim v ~ C.N2) => a -> a -> v a
mk2 a1 a2 = vector $ C.mk2 a1 a2
{-# INLINE mk2 #-}

mk3 :: (Vector v a, Dim v ~ C.N3) => a -> a -> a -> v a
mk3 a1 a2 a3 = vector $ C.mk3 a1 a2 a3
{-# INLINE mk3 #-}

mk4 :: (Vector v a, Dim v ~ C.N4) => a -> a -> a -> a -> v a
mk4 a1 a2 a3 a4 = vector $ C.mk4 a1 a2 a3 a4
{-# INLINE mk4 #-}

mk5 :: (Vector v a, Dim v ~ C.N5) => a -> a -> a -> a -> a -> v a
mk5 a1 a2 a3 a4 a5 = vector $ C.mk5 a1 a2 a3 a4 a5
{-# INLINE mk5 #-}



----------------------------------------------------------------
-- Generic functions
----------------------------------------------------------------

-- | Replicate value /n/ times.
--
--   Examples:
--
--   >>> import Data.Vector.Fixed.Boxed (Vec2)
--   >>> replicate 1 :: Vec2 Int
--   fromList [1,1]
--
--   >>> replicate 2 :: (Double,Double,Double)
--   (2.0,2.0,2.0)
--
--   >>> import Data.Vector.Fixed.Boxed (Vec4)
--   >>> replicate "foo" :: Vec4 String
--   fromList ["foo","foo","foo","foo"]
replicate :: Vector v a => a -> v a
{-# INLINE replicate #-}
replicate
  = vector . C.replicate


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
replicateM :: (Vector v a, Monad m) => m a -> m (v a)
{-# INLINE replicateM #-}
replicateM
  = liftM vector . C.replicateM


-- | Unit vector along Nth axis. If index is larger than vector
--   dimensions returns zero vector.
--
--   Examples:
--
--   >>> import Data.Vector.Fixed.Boxed (Vec3)
--   >>> basis 0 :: Vec3 Int
--   fromList [1,0,0]
--   >>> basis 1 :: Vec3 Int
--   fromList [0,1,0]
--   >>> basis 3 :: Vec3 Int
--   fromList [0,0,0]
basis :: (Vector v a, Num a) => Int -> v a
{-# INLINE basis #-}
basis = vector . C.basis


-- | Unfold vector.
unfoldr :: (Vector v a) => (b -> (a,b)) -> b -> v a
{-# INLINE unfoldr #-}
unfoldr f = vector . C.unfoldr f


-- | Generate vector from function which maps element's index to its
--   value.
--
--   Examples:
--
--   >>> import Data.Vector.Fixed.Unboxed (Vec4)
--   >>> generate (^2) :: Vec4 Int
--   fromList [0,1,4,9]
generate :: (Vector v a) => (Int -> a) -> v a
{-# INLINE generate #-}
generate = vector . C.generate


-- | Generate vector from monadic function which maps element's index
--   to its value.
generateM :: (Monad m, Vector v a) => (Int -> m a) -> m (v a)
{-# INLINE generateM #-}
generateM = liftM vector . C.generateM



----------------------------------------------------------------

-- | First element of vector.
--
--   Examples:
--
--   >>> import Data.Vector.Fixed.Boxed (Vec3)
--   >>> let x = mk3 1 2 3 :: Vec3 Int
--   >>> head x
--   1
head :: (Vector v a, Dim v ~ S n) => v a -> a
{-# INLINE head #-}
head = C.head . C.cvec


-- | Tail of vector.
--
--   Examples:
--
--   >>> import Data.Complex
--   >>> tail (1,2,3) :: Complex Double
--   2.0 :+ 3.0
tail :: (Vector v a, Vector w a, Dim v ~ S (Dim w))
     => v a -> w a
{-# INLINE tail #-}
tail = vector . C.tail . C.cvec

-- | Cons element to the vector
cons :: (Vector v a, Vector w a, S (Dim v) ~ Dim w)
     => a -> v a -> w a
{-# INLINE cons #-}
cons a = vector . C.cons a . C.cvec

-- | Append element to the vector
snoc :: (Vector v a, Vector w a, S (Dim v) ~ Dim w)
     => a -> v a -> w a
{-# INLINE snoc #-}
snoc a = vector . C.snoc a . C.cvec

-- | Reverse order of elements in the vector
reverse :: Vector v a => v a -> v a
reverse = vector . C.reverse . C.cvec
{-# INLINE reverse #-}

-- | Retrieve vector's element at index. Generic implementation is
--   /O(n)/ but more efficient one is used when possible.
(!) :: (Vector v a) => v a -> Int -> a
{-# INLINE (!) #-}
v ! n = runIndex n (C.cvec v)

-- Used in rewriting of index function.
runIndex :: Arity n => Int -> C.ContVec n r -> r
runIndex = C.index
{-# INLINE[0] runIndex #-}

-- | Get element from vector at statically known index
index :: (Vector v a, C.Index k (Dim v)) => v a -> k -> a
{-# INLINE index #-}
index v k = C.runContVec (C.getF k)
          $ C.cvec v  

-- | Twan van Laarhoven's lens for element of vector
element :: (Vector v a, Functor f) => Int -> (a -> f a) -> (v a -> f (v a))
{-# INLINE element #-}
element i f v = vector `fmap` C.element i f (C.cvec v)

-- | Twan van Laarhoven's lens for element of vector with statically
--   known index.
elementTy :: (Vector v a, Index k (Dim v), Functor f)
          => k -> (a -> f a) -> (v a -> f (v a))
{-# INLINE elementTy #-}
elementTy k f v = vector `fmap` C.elementTy k f (C.cvec v)



-- | Left fold over vector
foldl :: Vector v a => (b -> a -> b) -> b -> v a -> b
{-# INLINE foldl #-}
foldl f x = C.foldl f x
          . C.cvec

-- | Right fold over vector
foldr :: Vector v a => (a -> b -> b) -> b -> v a -> b
{-# INLINE foldr #-}
foldr f x = C.foldr f x
          . C.cvec


-- | Left fold over vector
foldl1 :: (Vector v a, Dim v ~ S n) => (a -> a -> a) -> v a -> a
{-# INLINE foldl1 #-}
foldl1 f = C.foldl1 f
         . C.cvec

-- | Combine the elements of a structure using a monoid. Similar to
--   'T.fold'
fold :: (Vector v m, Monoid m) => v m -> m
{-# INLINE fold #-}
fold = T.fold
     . C.cvec

-- | Map each element of the structure to a monoid,
--   and combine the results. Similar to 'T.foldMap'
foldMap :: (Vector v a, Monoid m) => (a -> m) -> v a -> m
{-# INLINE foldMap #-}
foldMap f = T.foldMap f
          . C.cvec

-- | Left fold over vector
ifoldr :: Vector v a => (Int -> a -> b -> b) -> b -> v a -> b
{-# INLINE ifoldr #-}
ifoldr f x = C.ifoldr f x
           . C.cvec

-- | Left fold over vector. Function is applied to each element and
--   its index.
ifoldl :: Vector v a => (b -> Int -> a -> b) -> b -> v a -> b
{-# INLINE ifoldl #-}
ifoldl f z = C.ifoldl f z
           . C.cvec

-- | Monadic fold over vector.
foldM :: (Vector v a, Monad m) => (b -> a -> m b) -> b -> v a -> m b
{-# INLINE foldM #-}
foldM f x = C.foldM f x . C.cvec

-- | Left monadic fold over vector. Function is applied to each element and
--   its index.
ifoldM :: (Vector v a, Monad m) => (b -> Int -> a -> m b) -> b -> v a -> m b
{-# INLINE ifoldM #-}
ifoldM f x = C.ifoldM f x . C.cvec



----------------------------------------------------------------

-- | Sum all elements in the vector.
sum :: (Vector v a, Num a) => v a -> a
sum = C.sum . C.cvec
{-# INLINE sum #-}

-- | Maximal element of vector.
--
--   Examples:
--
--   >>> import Data.Vector.Fixed.Boxed (Vec3)
--   >>> let x = mk3 1 2 3 :: Vec3 Int
--   >>> maximum x
--   3
maximum :: (Vector v a, Dim v ~ S n, Ord a) => v a -> a
maximum = C.maximum . C.cvec
{-# INLINE maximum #-}

-- | Minimal element of vector.
--
--   Examples:
--
--   >>> import Data.Vector.Fixed.Boxed (Vec3)
--   >>> let x = mk3 1 2 3 :: Vec3 Int
--   >>> minimum x
--   1
minimum :: (Vector v a, Dim v ~ S n, Ord a) => v a -> a
minimum = C.minimum . C.cvec
{-# INLINE minimum #-}

-- | Conjunction of all elements of a vector.
and :: (Vector v Bool) => v Bool -> Bool
and = C.and . C.cvec
{-# INLINE and #-}

-- | Disjunction of all elements of a vector.
or :: (Vector v Bool) => v Bool -> Bool
or = C.or . C.cvec
{-# INLINE or #-}

-- | Determines whether all elements of vector satisfy predicate.
all :: (Vector v a) => (a -> Bool) -> v a -> Bool
all f = (C.all f) . C.cvec
{-# INLINE all #-}

-- | Determines whether any of element of vector satisfy predicate.
any :: (Vector v a) => (a -> Bool) -> v a -> Bool
any f = (C.any f) . C.cvec
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
eq :: (Vector v a, Eq a) => v a -> v a -> Bool
{-# INLINE eq #-}
eq v w = C.and
       $ C.zipWith (==) (C.cvec v) (C.cvec w)


-- | Lexicographic ordering of two vectors.
ord :: (Vector v a, Ord a) => v a -> v a -> Ordering
{-# INLINE ord #-}
ord v w = C.foldl mappend mempty
        $ C.zipWith compare (C.cvec v) (C.cvec w)



----------------------------------------------------------------

-- | Map over vector
map :: (Vector v a, Vector v b) => (a -> b) -> v a -> v b
{-# INLINE map #-}
map f = vector
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
mapM f = liftM vector
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
imap f = vector
       . C.imap f
       . C.cvec

-- | Apply monadic function to every element of the vector and its index.
imapM :: (Vector v a, Vector v b, Monad m)
      => (Int -> a -> m b) -> v a -> m (v b)
{-# INLINE imapM #-}
imapM f = liftM vector
        . C.imapM f
        . C.cvec

-- | Apply monadic function to every element of the vector and its
--   index and discard result.
imapM_ :: (Vector v a, Monad m) => (Int -> a -> m b) -> v a -> m ()
{-# INLINE imapM_ #-}
imapM_ f = ifoldl (\m i a -> m >> f i a >> return ()) (return ())


-- | Analog of 'T.sequenceA' from 'T.Traversable'.
sequenceA :: (Vector v a, Vector v (f a), Applicative f)
          => v (f a) -> f (v a)
{-# INLINE sequenceA #-}
sequenceA = fmap vector . T.sequenceA . C.cvec

-- | Analog of 'T.traverse' from 'T.Traversable'.
traverse :: (Vector v a, Vector v b, Applicative f)
          => (a -> f b) -> v a -> f (v b)
{-# INLINE traverse #-}
traverse f = fmap vector . T.traverse f . C.cvec



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
zipWith :: (Vector v a, Vector v b, Vector v c)
        => (a -> b -> c) -> v a -> v b -> v c
{-# INLINE zipWith #-}
zipWith f v u = vector
              $ C.zipWith f (C.cvec v) (C.cvec u)

-- | Zip two vector together using monadic function.
zipWithM :: (Vector v a, Vector v b, Vector v c, Monad m)
         => (a -> b -> m c) -> v a -> v b -> m (v c)
{-# INLINE zipWithM #-}
zipWithM f v u = liftM vector
               $ C.zipWithM f (C.cvec v) (C.cvec u)

-- | Zip two vector together using function which takes element index
--   as well.
izipWith :: (Vector v a, Vector v b, Vector v c)
         => (Int -> a -> b -> c) -> v a -> v b -> v c
{-# INLINE izipWith #-}
izipWith f v u = vector
               $ C.izipWith f (C.cvec v) (C.cvec u)

-- | Zip two vector together using monadic function which takes element
--   index as well..
izipWithM :: (Vector v a, Vector v b, Vector v c, Monad m)
          => (Int -> a -> b -> m c) -> v a -> v b -> m (v c)
{-# INLINE izipWithM #-}
izipWithM f v u = liftM vector
                $ C.izipWithM f (C.cvec v) (C.cvec u)


----------------------------------------------------------------

-- | Convert between different vector types
convert :: (Vector v a, Vector w a, Dim v ~ Dim w) => v a -> w a
{-# INLINE convert #-}
convert = vector . C.cvec

-- | Convert vector to the list
toList :: (Vector v a) => v a -> [a]
toList = foldr (:) []
{-# INLINE toList #-}

-- | Create vector form list. Will throw error if list is shorter than
--   resulting vector.
fromList :: (Vector v a) => [a] -> v a
{-# INLINE fromList #-}
fromList = vector . C.fromList

-- | Create vector form list. Will throw error if list has different
--   length from resulting vector.
fromList' :: (Vector v a) => [a] -> v a
{-# INLINE fromList' #-}
fromList' = vector . C.fromList'

-- | Create vector form list. Will return @Nothing@ if list has different
--   length from resulting vector.
fromListM :: (Vector v a) => [a] -> Maybe (v a)
{-# INLINE fromListM #-}
fromListM = liftM vector . C.fromListM

-- | Create vector from 'Foldable' data type. Will return @Nothing@ if
--   data type different number of elements that resulting vector.
fromFoldable :: (Vector v a, T.Foldable f) => f a -> Maybe (v a)
{-# INLINE fromFoldable #-}
fromFoldable = fromListM . T.toList
