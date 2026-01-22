{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE MagicHash           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE UnboxedTuples       #-}
module Data.Vector.Fixed.Mono
  ( -- * Vector type class
    Prod(..)
  , Vector
  , Dim
  , C.Arity
  , C.ArityPeano
  , C.Fun(..)
  , length
    -- ** Peano numbers
  , PeanoNum(..)
  , Peano
  , N1, N2, N3, N4, N5, N6, N7, N8
    -- * Construction and destructions
    -- $construction

    -- ** Constructors
  , mk0
  , mk1
  , mk2
  , mk3
  , mk4
  , mk5
  , mk6
  , mk7
  , mk8
  , mkN
    -- ** Pattern synonyms
  , pattern V1
  , pattern V2
  , pattern V3
  , pattern V4
    -- * Functions
    -- ** Creation
  , replicate
  , replicateM
  , generate
  , generateM
  , unfoldr
  , basis
    -- ** Transformations
  , head
  , tail
  , cons
  , snoc
  , concat
  , reverse
    -- ** Indexing & lenses
  , C.Index
  , (!)
  , index
  , set
  , element
  , elementTy
    -- ** Maps
  , map
  , gmap
  , mapM
  , gmapM
  , mapM_
  , imap
  , igmap
  , imapM
  , igmapM
  , imapM_
  , scanl
  , scanl1
  -- , traverse
    -- ** Folds
  , foldl
  , foldl'
  , foldr
  , foldl1
  , fold
  , foldMap
  , ifoldl
  , ifoldl'
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
  , find
    -- ** Zips
  , zipWith
  , zipWith3
  , zipWithM
  , zipWithM_
  , izipWith
  , izipWith3
  , izipWithM
  , izipWithM_
    -- *** Special zips
  , eq
  , ord
    -- ** Conversion
  , convert
  , toList
  , fromList
  , fromList'
  , fromListM
  , fromFoldable
    -- ** Continuation-based vectors
  , C.ContVec
  , vector
  , cvec
    -- * Instance deriving
  , ViaFixed(..)
  ) where

import Control.DeepSeq         (NFData(..))
import Control.Monad.Primitive (PrimBase(..))
import Data.Complex
import Data.Foldable           qualified as T
import Data.Primitive.Types    (Prim(..))
import Foreign.Ptr             (castPtr)
import Foreign.Storable        (Storable(..))

import GHC.Exts (Proxy#,proxy#,Int(..),Int#,(+#),(*#))
import GHC.ST   (ST(..))

import Prelude (Eq(..),Ord(..),Show(..),Num(..),Functor,Applicative,Monad
               ,Semigroup(..),Monoid(..)
               ,Bool,Maybe(..),Ordering
               ,fmap,(<$>),(.),($),shows,flip,undefined
               )

import Data.Vector.Fixed.Compat
import Data.Vector.Fixed.Cont qualified as C
import Data.Vector.Fixed.Cont (Dim,Add,ArityPeano,Peano,Index,PeanoNum(..),
                               N1,N2,N3,N4,N5,N6,N7,N8)



----------------------------------------------------------------
-- Classes
----------------------------------------------------------------


class C.ArityPeano (Dim v) => Prod a v | v -> a where
  inspect   :: v -> C.Fun (Dim v) a r -> r
  construct :: C.Fun (Dim v) a v

class Prod a v => Vector a v

-- | Convert regular vector to continuation based one.
cvec :: (Prod a v) => v -> C.ContVec (Dim v) a
cvec v = C.ContVec (inspect v)
{-# INLINE[0] cvec #-}

-- | Convert continuation to the vector.
vector :: (Prod a v) => C.ContVec (Dim v) a -> v
vector = C.runContVec construct
{-# INLINE[1] vector #-}

{-# RULES
"cvec/vector[mono]" forall v.
  cvec (vector v) = v
  #-}



----------------------------------------------------------------
-- Constructors
----------------------------------------------------------------

mk0 :: forall v a. (Vector a v, Dim v ~ 'Z) => v
mk0 = vector C.empty
{-# INLINE mk0 #-}

mk1 :: forall v a. (Vector a v, Dim v ~ N1) => a -> v
mk1 a1 = vector $ C.mk1 a1
{-# INLINE mk1 #-}

mk2 :: forall v a. (Vector a v, Dim v ~ N2) => a -> a -> v
mk2 a1 a2 = vector $ C.mk2 a1 a2
{-# INLINE mk2 #-}

mk3 :: forall v a. (Vector a v, Dim v ~ N3) => a -> a -> a -> v
mk3 a1 a2 a3 = vector $ C.mk3 a1 a2 a3
{-# INLINE mk3 #-}

mk4 :: forall v a. (Vector a v, Dim v ~ N4) => a -> a -> a -> a -> v
mk4 a1 a2 a3 a4 = vector $ C.mk4 a1 a2 a3 a4
{-# INLINE mk4 #-}

mk5 :: forall v a. (Vector a v, Dim v ~ N5) => a -> a -> a -> a -> a -> v
mk5 a1 a2 a3 a4 a5 = vector $ C.mk5 a1 a2 a3 a4 a5
{-# INLINE mk5 #-}

mk6 :: forall v a. (Vector a v, Dim v ~ N6) => a -> a -> a -> a -> a -> a -> v
mk6 a1 a2 a3 a4 a5 a6 = vector $ C.mk6 a1 a2 a3 a4 a5 a6
{-# INLINE mk6 #-}

mk7 :: forall v a. (Vector a v, Dim v ~ N7) => a -> a -> a -> a -> a -> a -> a -> v
mk7 a1 a2 a3 a4 a5 a6 a7 = vector $ C.mk7 a1 a2 a3 a4 a5 a6 a7
{-# INLINE mk7 #-}

mk8 :: forall v a. (Vector a v, Dim v ~ N8) => a -> a -> a -> a -> a -> a -> a -> a -> v
mk8 a1 a2 a3 a4 a5 a6 a7 a8 = vector $ C.mk8 a1 a2 a3 a4 a5 a6 a7 a8
{-# INLINE mk8 #-}

-- | N-ary constructor. Despite scary signature it's just N-ary
--   function with additional type parameter which is used to fix type
--   of vector being constructed. It could be used as:
--
--   > v = mkN (Proxy :: Proxy (Int,Int,Int)) 1 2 3
--
--   or using @TypeApplications@ syntax:
--
--   > v = mkN (Proxy @(Int,Int,Int)) 1 2 3
--
--   or if type of @v@ is fixed elsewhere
--
--   > v = mkN [v] 1 2 3
mkN :: forall proxy v a. (Vector a v)
    => proxy v -> C.Fn (Dim v) a v
mkN _ = C.unFun (construct :: C.Fun (Dim v) a v)

----------------------------------------------------------------
-- Generic functions
----------------------------------------------------------------

-- | Length of vector. Function doesn't evaluate its argument.
length :: forall v. C.ArityPeano (Dim v) => v -> Int
{-# INLINE length #-}
length _ = C.peanoToInt (proxy# @(Dim v))

-- | Replicate value /n/ times.
--
--   Examples:
--
--   >>> import Data.Vector.Fixed.Boxed (Vec2)
--   >>> replicate 1 :: Vec2 Int
--   [1,1]
--
--   >>> replicate 2 :: (Double,Double,Double)
--   (2.0,2.0,2.0)
--
--   >>> import Data.Vector.Fixed.Boxed (Vec4)
--   >>> replicate "foo" :: Vec4 String
--   ["foo","foo","foo","foo"]
replicate :: forall v a. Vector a v => a -> v
{-# INLINE replicate #-}
replicate
  = vector . C.replicate


-- | Execute monadic action for every element of vector.
--
--   Examples:
--
--   >>> import Data.Vector.Fixed.Boxed (Vec2,Vec3)
--   >>> replicateM (Just 3) :: Maybe (Vec3 Int)
--   Just [3,3,3]
--   >>> replicateM (putStrLn "Hi!") :: IO (Vec2 ())
--   Hi!
--   Hi!
--   [(),()]
replicateM :: forall v f a. (Vector a v, Applicative f) => f a -> f (v)
{-# INLINE replicateM #-}
replicateM
  = fmap vector . C.replicateM


-- | Unit vector along Nth axis. If index is larger than vector
--   dimensions returns zero vector.
--
--   Examples:
--
--   >>> import Data.Vector.Fixed.Boxed (Vec3)
--   >>> basis 0 :: Vec3 Int
--   [1,0,0]
--   >>> basis 1 :: Vec3 Int
--   [0,1,0]
--   >>> basis 3 :: Vec3 Int
--   [0,0,0]
basis :: forall v a. (Vector a v, Num a) => Int -> v
{-# INLINE basis #-}
basis = vector . C.basis


-- | Unfold vector.
unfoldr :: forall v a b. (Vector a v) => (b -> (a,b)) -> b -> v
{-# INLINE unfoldr #-}
unfoldr f = vector . C.unfoldr f


-- | Generate vector from function which maps element's index to its
--   value.
--
--   Examples:
--
--   >>> import Data.Vector.Fixed.Unboxed (Vec4)
--   >>> generate (^2) :: Vec4 Int
--   [0,1,4,9]
generate :: forall v a. (Vector a v) => (Int -> a) -> v
{-# INLINE generate #-}
generate = vector . C.generate


-- | Generate vector from monadic function which maps element's index
--   to its value.
generateM :: forall v f a. (Applicative f, Vector a v) => (Int -> f a) -> f v
{-# INLINE generateM #-}
generateM = fmap vector . C.generateM



----------------------------------------------------------------

-- | First element of vector.
--
--   Examples:
--
--   >>> import Data.Vector.Fixed.Boxed (Vec3)
--   >>> let x = mk3 1 2 3 :: Vec3 Int
--   >>> head x
--   1
head :: forall v k a. (Vector a v, Dim v ~ 'S k) => v -> a
{-# INLINE head #-}
head = C.head . cvec


-- | Tail of vector.
--
--   Examples:
--
--   >>> import Data.Complex
--   >>> tail (1,2,3) :: Complex Double
--   2.0 :+ 3.0
tail :: forall v w a. (Vector a v, Vector a w, Dim v ~ 'S (Dim w))
     => v -> w
{-# INLINE tail #-}
tail = vector . C.tail . cvec

-- | Cons element to the vector
cons :: forall v w a. (Vector a v, Vector a w, Dim w ~ 'S (Dim v))
     => a -> v -> w
{-# INLINE cons #-}
cons a = vector . C.cons a . cvec

-- | Append element to the vector
snoc :: forall v w a. (Vector a v, Vector a w, Dim w ~ 'S (Dim v))
     => a -> v -> w
{-# INLINE snoc #-}
snoc a = vector . C.snoc a . cvec

concat :: forall v u w a.
  ( Vector a v, Vector a u, Vector a w
  , (Dim v `Add` Dim u) ~ Dim w
  )
  => v -> u -> w
{-# INLINE concat #-}
concat v u = vector $ C.concat (cvec v) (cvec u)

-- | Reverse order of elements in the vector
reverse :: forall v a. Vector a v => v -> v
reverse = vector . C.reverse . cvec
{-# INLINE reverse #-}


-- | Retrieve vector's element at index. Generic implementation is
--   /O(n)/ but more efficient one is used when possible.
(!) :: forall v a. (Vector a v) => v -> Int -> a
{-# INLINE (!) #-}
v ! i = C.index i (cvec v)

-- | Get element from vector at statically known index
index :: forall v k a proxy. (Vector a v, Index (Peano k) (Dim v))
      => v -> proxy k -> a
{-# INLINE index #-}
index v _ = inspect v (C.getF (proxy# @(Peano k)))

-- | Set n'th element in the vector
set :: forall v k a proxy. (Vector a v, Index (Peano k) (Dim v))
    => proxy k -> a -> v -> v
{-# INLINE set #-}
set _ a v
  = inspect v
  $ C.putF (proxy# @(Peano k)) a construct

-- | Twan van Laarhoven's lens for element of vector
element :: forall v f a. (Vector a v, Functor f) => Int -> (a -> f a) -> (v -> f v)
{-# INLINE element #-}
element i f v = vector `fmap` C.element i f (cvec v)

-- | Twan van Laarhoven's lens for element of vector with statically
--   known index.
elementTy
  :: forall v f k a proxy. (Vector a v, Index (Peano k) (Dim v), Functor f)
  => proxy k -> (a -> f a) -> (v -> f v)
{-# INLINE elementTy #-}
elementTy _ f v
  = inspect v (C.lensF (proxy# @(Peano k)) f construct)

-- | Left fold over vector
foldl :: forall v b a. Vector a v => (b -> a -> b) -> b -> v -> b
{-# INLINE foldl #-}
foldl f x = C.foldl f x
          . cvec

-- | Strict left fold over vector
foldl' :: forall v b a. Vector a v => (b -> a -> b) -> b -> v -> b
{-# INLINE foldl' #-}
foldl' f x = C.foldl' f x
           . cvec

-- | Right fold over vector
foldr :: forall v b a. Vector a v => (a -> b -> b) -> b -> v -> b
{-# INLINE foldr #-}
foldr f x = C.foldr f x
          . cvec


-- | Left fold over vector
foldl1 :: forall v a k. (Vector a v, Dim v ~ 'S k) => (a -> a -> a) -> v -> a
{-# INLINE foldl1 #-}
foldl1 f = C.foldl1 f
         . cvec

-- | Combine the elements of a structure using a monoid. Similar to
--   'T.fold'
fold :: forall v m. (Vector m v, Monoid m) => v -> m
{-# INLINE fold #-}
fold = T.fold
     . cvec

-- | Map each element of the structure to a monoid,
--   and combine the results. Similar to 'T.foldMap'
foldMap :: forall v m a. (Vector a v, Monoid m) => (a -> m) -> v -> m
{-# INLINE foldMap #-}
foldMap f = T.foldMap f
          . cvec

-- | Right fold over vector
ifoldr :: forall v b a. Vector a v => (Int -> a -> b -> b) -> b -> v -> b
{-# INLINE ifoldr #-}
ifoldr f x = C.ifoldr f x
           . cvec

-- | Left fold over vector. Function is applied to each element and
--   its index.
ifoldl :: forall v b a. Vector a v => (b -> Int -> a -> b) -> b -> v -> b
{-# INLINE ifoldl #-}
ifoldl f z = C.ifoldl f z
           . cvec

-- | Strict left fold over vector. Function is applied to each element
--   and its index.
ifoldl' :: forall v b a. Vector a v => (b -> Int -> a -> b) -> b -> v -> b
{-# INLINE ifoldl' #-}
ifoldl' f z = C.ifoldl' f z
            . cvec

-- | Monadic fold over vector.
foldM :: forall v m b a. (Vector a v, Monad m) => (b -> a -> m b) -> b -> v -> m b
{-# INLINE foldM #-}
foldM f x = C.foldM f x . cvec

-- | Left monadic fold over vector. Function is applied to each element and
--   its index.
ifoldM :: forall v m b a. (Vector a v, Monad m) => (b -> Int -> a -> m b) -> b -> v -> m b
{-# INLINE ifoldM #-}
ifoldM f x = C.ifoldM f x . cvec


----------------------------------------------------------------

-- | Sum all elements in the vector.
sum :: forall v a. (Vector a v, Num a) => v -> a
sum = C.sum . cvec
{-# INLINE sum #-}

-- | Maximal element of vector.
--
--   Examples:
--
--   >>> import Data.Vector.Fixed.Boxed (Vec3)
--   >>> let x = mk3 1 2 3 :: Vec3 Int
--   >>> maximum x
--   3
maximum :: forall v a k. (Vector a v, Dim v ~ S k, Ord a) => v -> a
maximum = C.maximum . cvec
{-# INLINE maximum #-}

-- | Minimal element of vector.
--
--   Examples:
--
--   >>> import Data.Vector.Fixed.Boxed (Vec3)
--   >>> let x = mk3 1 2 3 :: Vec3 Int
--   >>> minimum x
--   1
minimum :: forall v a k. (Vector a v, Dim v ~ S k, Ord a) => v -> a
minimum = C.minimum . cvec
{-# INLINE minimum #-}

-- | Conjunction of all elements of a vector.
and :: forall v. (Vector Bool v) => v -> Bool
and = C.and . cvec
{-# INLINE and #-}

-- | Disjunction of all elements of a vector.
or :: forall v. (Vector Bool v) => v -> Bool
or = C.or . cvec
{-# INLINE or #-}

-- | Determines whether all elements of vector satisfy predicate.
all :: forall v a. (Vector a v) => (a -> Bool) -> v -> Bool
all f = (C.all f) . cvec
{-# INLINE all #-}

-- | Determines whether any of element of vector satisfy predicate.
any :: forall v a. (Vector a v) => (a -> Bool) -> v -> Bool
any f = (C.any f) . cvec
{-# INLINE any #-}

-- | The 'find' function takes a predicate and a vector and returns
--   the leftmost element of the vector matching the predicate,
--   or 'Nothing' if there is no such element.
find :: forall v a. (Vector a v) => (a -> Bool) -> v -> Maybe a
find f = (C.find f) . cvec
{-# INLINE find #-}

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
eq :: (Vector a v, Eq a) => v -> v -> Bool
{-# INLINE eq #-}
eq v w = C.and
       $ C.zipWith (==) (cvec v) (cvec w)


-- | Lexicographic ordering of two vectors.
ord :: (Vector a v, Ord a) => v -> v -> Ordering
{-# INLINE ord #-}
ord v w = C.foldl mappend mempty
        $ C.zipWith compare (cvec v) (cvec w)


----------------------------------------------------------------

-- | Map over vector
map :: forall v a. (Vector a v) => (a -> a) -> v -> v
{-# INLINE map #-}
map f = vector
      . C.map f
      . cvec

-- | Map over vector
gmap :: forall v w a b. (Vector a v, Vector b w, Dim v ~ Dim w) => (a -> b) -> v -> w
{-# INLINE gmap #-}
gmap f = vector
       . C.map f
       . cvec

-- | Effectful map over vector.
mapM :: forall v f a. (Vector a v, Applicative f) => (a -> f a) -> v -> f v
{-# INLINE mapM #-}
mapM f = fmap vector
       . C.mapM f
       . cvec

-- | Effectful map over vector.
gmapM :: forall v w f a b. (Vector a v, Vector b w, Applicative f, Dim v ~ Dim w)
      => (a -> f b) -> v -> f w
{-# INLINE gmapM #-}
gmapM f = fmap vector
        . C.mapM f
        . cvec

-- | Apply monadic action to each element of vector and ignore result.
mapM_ :: forall v f b a. (Vector a v, Applicative f) => (a -> f b) -> v -> f ()
{-# INLINE mapM_ #-}
mapM_ f = C.mapM_ f
        . cvec


-- | Apply function to every element of the vector and its index.
imap :: forall v a. (Vector a v) => (Int -> a -> a) -> v -> v
{-# INLINE imap #-}
imap f = vector
       . C.imap f
       . cvec

-- | Apply function to every element of the vector and its index.
igmap :: forall v w a b. (Vector a v, Vector b w, Dim v ~ Dim w)
      => (Int -> a -> b) -> v -> w
{-# INLINE igmap #-}
igmap f = vector
       . C.imap f
       . cvec

-- | Apply monadic function to every element of the vector and its index.
imapM :: forall v f a. (Vector a v, Applicative f)
      => (Int -> a -> f a) -> v -> f v
{-# INLINE imapM #-}
imapM f = fmap vector
        . C.imapM f
        . cvec

-- | Apply monadic function to every element of the vector and its index.
igmapM :: forall v w f a b. (Vector a v, Vector b w, Dim v ~ Dim w, Applicative f)
       => (Int -> a -> f b) -> v -> f w
{-# INLINE igmapM #-}
igmapM f = fmap vector
         . C.imapM f
         . cvec

-- | Apply monadic function to every element of the vector and its
--   index and discard result.
imapM_ :: forall v f b a. (Vector a v, Applicative f) => (Int -> a -> f b) -> v -> f ()
{-# INLINE imapM_ #-}
imapM_ f = C.imapM_ f
         . cvec

-- | Left scan over vector
scanl :: forall v w a b. (Vector a v, Vector b w, Dim w ~ 'S (Dim v))
      => (b -> a -> b) -> b -> v -> w
{-# INLINE scanl #-}
scanl f x0 = vector . C.scanl f x0 . cvec

-- | Left scan over vector
scanl1 :: forall v a. (Vector a v)
      => (a -> a -> a) -> v -> v
{-# INLINE scanl1 #-}
scanl1 f = vector . C.scanl1 f . cvec



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
--   [1,1,0]
--   >>> vplus b0 b2
--   [1,0,1]
--   >>> vplus b1 b2
--   [0,1,1]
zipWith :: forall v a. (Vector a v)
        => (a -> a -> a) -> v -> v -> v
{-# INLINE zipWith #-}
zipWith f v u = vector
              $ C.zipWith f (cvec v) (cvec u)

-- | Zip three vector together
zipWith3
  :: forall v a. (Vector a v)
  => (a -> a -> a -> a)
  -> v -> v -> v -> v
{-# INLINE zipWith3 #-}
zipWith3 f v1 v2 v3
  = vector
  $ C.zipWith3 f (cvec v1) (cvec v2) (cvec v3)

-- | Zip two vector together using monadic function.
zipWithM :: forall v f a. (Vector a v, Applicative f)
         => (a -> a -> f a) -> v -> v -> f v
{-# INLINE zipWithM #-}
zipWithM f v u = fmap vector
               $ C.zipWithM f (cvec v) (cvec u)

-- | Zip two vector elementwise using monadic function and discard
--   result
zipWithM_
  :: forall v f b a. (Vector a v, Applicative f)
  => (a -> a -> f b) -> v -> v -> f ()
{-# INLINE zipWithM_ #-}
zipWithM_ f xs ys = C.zipWithM_ f (cvec xs) (cvec ys)

-- | Zip two vector together using function which takes element index
--   as well.
izipWith :: forall v a. (Vector a v)
         => (Int -> a -> a -> a) -> v -> v -> v
{-# INLINE izipWith #-}
izipWith f v u = vector
               $ C.izipWith f (cvec v) (cvec u)

-- | Zip three vector together
izipWith3
  :: forall v a. (Vector a v)
  => (Int -> a -> a -> a -> a)
  -> v -> v -> v
  -> v
{-# INLINE izipWith3 #-}
izipWith3 f v1 v2 v3
  = vector
  $ C.izipWith3 f (cvec v1) (cvec v2) (cvec v3)

-- | Zip two vector together using monadic function which takes element
--   index as well..
izipWithM :: forall v f a. (Vector a v, Applicative f)
          => (Int -> a -> a -> f a) -> v -> v -> f v
{-# INLINE izipWithM #-}
izipWithM f v u = fmap vector
                $ C.izipWithM f (cvec v) (cvec u)

-- | Zip two vector elementwise using monadic function and discard
--   result
izipWithM_
  :: forall v f b a. (Vector a v, Applicative f)
  => (Int -> a -> a -> f b) -> v -> v -> f ()
{-# INLINE izipWithM_ #-}
izipWithM_ f xs ys = C.izipWithM_ f (cvec xs) (cvec ys)


----------------------------------------------------------------

-- | Convert between different vector types
convert :: forall v w a. (Vector a v, Vector a w, Dim v ~ Dim w) => v -> w
{-# INLINE convert #-}
convert = vector . cvec

-- | Convert vector to the list
toList :: forall v a. (Vector a v) => v -> [a]
toList = foldr (:) []
{-# INLINE toList #-}

-- | Create vector form list. Will throw error if list is shorter than
--   resulting vector.
fromList :: forall v a. (Vector a v) => [a] -> v
{-# INLINE fromList #-}
fromList = vector . C.fromList

-- | Create vector form list. Will throw error if list has different
--   length from resulting vector.
fromList' :: forall v a. (Vector a v) => [a] -> v
{-# INLINE fromList' #-}
fromList' = vector . C.fromList'

-- | Create vector form list. Will return @Nothing@ if list has different
--   length from resulting vector.
fromListM :: forall v a. (Vector a v) => [a] -> Maybe v
{-# INLINE fromListM #-}
fromListM = fmap vector . C.fromListM

-- | Create vector from 'Foldable' data type. Will return @Nothing@ if
--   data type different number of elements that resulting vector.
fromFoldable :: forall v f a. (Vector a v, T.Foldable f) => f a -> Maybe v
{-# INLINE fromFoldable #-}
fromFoldable = fromListM . T.toList




----------------------------------------------------------------
--
----------------------------------------------------------------

-- | Newtype for deriving instances.
newtype ViaFixed a v = ViaFixed v

instance (Prod a v) => Prod a (ViaFixed a v) where
  inspect (ViaFixed v) = inspect v
  construct = ViaFixed <$> construct
instance (Prod a v) => Vector a (ViaFixed a v)

type instance Dim (ViaFixed a v) = Dim v

instance (Prod a v, Show a) => Show (ViaFixed a v) where
  showsPrec _ = shows . toList

instance (Prod a v, Eq a) => Eq (ViaFixed a v) where
  (==) = eq
  {-# INLINE (==) #-}

instance (Prod a v, Ord a) => Ord (ViaFixed a v) where
  compare = ord
  {-# INLINE compare #-}

instance (Prod a v, NFData a) => NFData (ViaFixed a v) where
  rnf = foldl (\() a -> rnf a) ()
  {-# INLINE rnf #-}

instance (Prod a v, Semigroup a) => Semigroup (ViaFixed a v) where
  (<>) = zipWith (<>)
  {-# INLINE (<>) #-}

instance (Prod a v, Monoid a) => Monoid (ViaFixed a v) where
  mempty = replicate mempty
  {-# INLINE mempty #-}

instance (Prod a v, Storable a) => Storable (ViaFixed a v) where
  alignment _ = alignment (undefined :: a)
  sizeOf    _ = sizeOf (undefined :: a) * C.peanoToInt (proxy# @(Dim v))
  peek p = generateM (peekElemOff (castPtr p))
  poke p = imapM_    (pokeElemOff (castPtr p))
  {-# INLINE alignment #-}
  {-# INLINE sizeOf    #-}
  {-# INLINE peek      #-}
  {-# INLINE poke      #-}

instance (Prod a v, Prim a) => Prim (ViaFixed a v) where
  sizeOf# _ = sizeOf# (undefined :: a) *# dim where
    dim = case C.peanoToInt (proxy# @(Dim v)) of I# i -> i
  alignment# _ = alignment# (undefined :: a)
  {-# INLINE sizeOf#    #-}
  {-# INLINE alignment# #-}
  -- Bytearray
  indexByteArray# ba k
    = generate $ \(I# i) -> indexByteArray# ba (off +# i)
    where
      off = vectorOff (proxy# @(Dim v))  k
  readByteArray# ba k
    = internal
    $ generateM
    $ \(I# i) -> ST (\s -> readByteArray# ba (off +# i) s)
    where
      off = vectorOff (proxy# @(Dim v))  k
  writeByteArray# ba k vec =
    case loop of
      ST st -> \s -> case st s of
                       (# s', () #) -> s'
    where
      off  = vectorOff (proxy# @(Dim v))  k
      loop = flip imapM_ vec $ \(I# i) a -> ST $ \s ->
        (# writeByteArray# ba (off +# i) a s, () #)
  {-# INLINE indexByteArray# #-}
  {-# INLINE readByteArray#  #-}
  {-# INLINE writeByteArray# #-}
  -- Addr
  indexOffAddr# addr k
    = generate $ \(I# i) -> indexOffAddr# addr (off +# i)
    where
      off = vectorOff (proxy# @(Dim v))  k
  readOffAddr# ba k
    = internal
    $ generateM
    $ \(I# i) -> ST (\s -> readOffAddr# ba (off +# i) s)
    where
      off = vectorOff (proxy# @(Dim v))  k
  writeOffAddr# addr k vec =
    case loop of
      ST st -> \s -> case st s of
                       (# s', () #) -> s'
    where
      off  = vectorOff (proxy# @(Dim v))  k
      loop = flip imapM_ vec $ \(I# i) a -> ST $ \s ->
        (# writeOffAddr# addr (off +# i) a s, () #)
  {-# INLINE indexOffAddr# #-}
  {-# INLINE readOffAddr#  #-}
  {-# INLINE writeOffAddr# #-}


vectorOff :: (ArityPeano n) => Proxy# n -> Int# -> Int#
{-# INLINE vectorOff #-}
vectorOff n k =
  case C.peanoToInt n of
    I# dim -> dim *# k

----------------------------------------------------------------
-- Patterns
----------------------------------------------------------------

pattern V1 :: (Vector a v, Dim v ~ N1) => a -> v
pattern V1 x <- (head -> x) where
  V1 x = mk1 x
{-# INLINE   V1 #-}
{-# COMPLETE V1 #-}

pattern V2 :: (Vector a v, Dim v ~ N2) => a -> a -> v
pattern V2 x y <- (convert -> (x,y)) where
  V2 x y = mk2 x y
{-# INLINE   V2 #-}
{-# COMPLETE V2 #-}

pattern V3 :: (Vector a v, Dim v ~ N3) => a -> a -> a -> v
pattern V3 x y z <- (convert -> (x,y,z)) where
  V3 x y z = mk3 x y z
{-# INLINE   V3 #-}
{-# COMPLETE V3 #-}

pattern V4 :: (Vector a v, Dim v ~ N4) => a -> a -> a -> a -> v
pattern V4 t x y z <- (convert -> (t,x,y,z)) where
  V4 t x y z = mk4 t x y z
{-# INLINE   V4 #-}
{-# COMPLETE V4 #-}

----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

instance Prod a (Complex a) where
  inspect (r :+ i) (C.Fun f) = f r i
  construct = C.Fun (:+)
  {-# INLINE inspect   #-}
  {-# INLINE construct #-}
instance Vector a (Complex a)

instance (a1 ~ a2) => Prod a1 (a1, a2) where
  inspect (a1, a2) (C.Fun f) = f a1 a2
  construct = C.Fun (,)
  {-# INLINE inspect   #-}
  {-# INLINE construct #-}

instance (a1 ~ a2, a2 ~ a3) => Prod a1 (a1, a2, a3) where
  inspect (a1, a2, a3) (C.Fun f) = f a1 a2 a3
  construct = C.Fun (,,)
  {-# INLINE inspect   #-}
  {-# INLINE construct #-}

instance (a1 ~ a2, a2 ~ a3, a3 ~ a4) => Prod a1 (a1, a2, a3, a4) where
  inspect (a1, a2, a3, a4) (C.Fun f) = f a1 a2 a3 a4
  construct = C.Fun (,,,)
  {-# INLINE inspect   #-}
  {-# INLINE construct #-}

instance (a1 ~ a2, a2 ~ a3, a3 ~ a4, a4 ~ a5) => Prod a1 (a1, a2, a3, a4, a5) where
  inspect (a1, a2, a3, a4, a5) (C.Fun f) = f a1 a2 a3 a4 a5
  construct = C.Fun (,,,,)
  {-# INLINE inspect   #-}
  {-# INLINE construct #-}

instance (a1 ~ a2, a2 ~ a3, a3 ~ a4, a4 ~ a5, a5 ~ a6
         ) => Prod a1 (a1, a2, a3, a4, a5, a6) where
  inspect (a1, a2, a3, a4, a5, a6) (C.Fun f) = f a1 a2 a3 a4 a5 a6
  construct = C.Fun (,,,,,)
  {-# INLINE inspect   #-}
  {-# INLINE construct #-}

instance (a1 ~ a2, a2 ~ a3, a3 ~ a4, a4 ~ a5, a5 ~ a6, a6 ~ a7
         ) => Prod a1 (a1, a2, a3, a4, a5, a6, a7) where
  inspect (a1, a2, a3, a4, a5, a6, a7) (C.Fun f) = f a1 a2 a3 a4 a5 a6 a7
  construct = C.Fun (,,,,,,)
  {-# INLINE inspect   #-}
  {-# INLINE construct #-}



instance (a1 ~ a2) => Vector a1 (a1, a2)
instance (a1 ~ a2, a2 ~ a3) => Vector a1 (a1, a2, a3)
instance (a1 ~ a2, a2 ~ a3, a3 ~ a4) => Vector a1 (a1, a2, a3, a4)
instance (a1 ~ a2, a2 ~ a3, a3 ~ a4, a4 ~ a5) => Vector a1 (a1, a2, a3, a4, a5)
instance (a1 ~ a2, a2 ~ a3, a3 ~ a4, a4 ~ a5, a5 ~ a6
         ) => Vector a1 (a1, a2, a3, a4, a5, a6)
instance (a1 ~ a2, a2 ~ a3, a3 ~ a4, a4 ~ a5, a5 ~ a6, a6 ~ a7
         ) => Vector a1 (a1, a2, a3, a4, a5, a6, a7)


-- $setup
--
-- >>> import Data.Char
-- >>> import Prelude (Int,Bool(..),Double,IO,(^),String,putStrLn)

