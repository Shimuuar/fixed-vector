{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
-- |
-- Type classes for vectors which are implemented on top of the arrays
-- and support in-place mutation. API is similar to one used in the
-- @vector@ package.
module Data.Vector.Fixed.Mutable (
    -- * Mutable vectors
    Arity
  , Mutable
  , DimM
  , MVector(..)
  , lengthM
  , new
  , clone
  , copy
  , read
  , write
  , unsafeRead
  , unsafeWrite
    -- * Creation
  , replicate
  , replicateM
  , generate
  , generateM
    -- * Loops
  , forI
    -- * Immutable vectors
  , IVector(..)
  , index
  , freeze
  , thaw
  , unsafeFreeze
  , unsafeThaw
    -- * Vector API
  , constructVec
  , inspectVec
  ) where

import Control.Applicative  (Const(..))
import Control.Monad.ST
import Control.Monad.Primitive
import Data.Kind      (Type)
import Prelude hiding (read,length,replicate)
import GHC.Exts (proxy#)

import Data.Vector.Fixed.Cont (Dim,PeanoNum(..),Arity,ArityPeano(..),Fun(..),Vector(..),
                               ContVec,apply,accum,length)


----------------------------------------------------------------
-- Type classes
----------------------------------------------------------------

-- | Mutable counterpart of fixed-length vector.
type family Mutable (v :: Type -> Type) :: Type -> Type -> Type

-- | Dimension for mutable vector.
type family DimM (v :: Type -> Type -> Type) :: PeanoNum

-- | Type class for mutable vectors.
class (ArityPeano (DimM v)) => MVector v a where
  -- | Copy vector. The two vectors may not overlap. Shouldn't be used
  --   directly, use 'copy' instead.
  basicCopy :: v s a    -- ^ Target
            -> v s a    -- ^ Source
            -> ST s ()
  -- | Allocate new uninitialized vector. Shouldn't be used
  --   directly, use 'new' instead.
  basicNew :: ST s (v s a)
  -- | Allocate new vector initialized with given element. Shouldn't be used
  --   directly, use 'replicate' instead.
  basicReplicate :: a -> ST s (v s a)
  {-# INLINE basicReplicate #-}
  basicReplicate a = do
    v <- basicNew
    forI v $ \i -> basicUnsafeWrite v i a
    pure v
  -- | Create copy of existing vector. Shouldn't be used
  --   directly, use 'clone' instead.
  basicClone :: v s a -> ST s (v s a)
  {-# INLINE basicClone #-}
  basicClone src = do
    dst <- basicNew
    basicCopy dst src
    pure src
  -- | Read value at index without bound checks. Shouldn't be used
  --   directly, use 'unsafeRead' instead.
  basicUnsafeRead  :: v s a -> Int -> ST s a
  -- | Write value at index without bound checks. Shouldn't be used
  --   directly, use 'unsafeWrite' instead.
  basicUnsafeWrite :: v s a -> Int -> a -> ST s ()

-- | Length of mutable vector. Function doesn't evaluate its argument.
lengthM :: forall v s a. (ArityPeano (DimM v)) => v s a -> Int
lengthM _ = peanoToInt (proxy# @(DimM v))

-- | Create new uninitialized  mutable vector.
new :: (MVector v a, PrimMonad m) => m (v (PrimState m) a)
new = stToPrim basicNew
{-# INLINE new #-}

-- | Copy vector. The two vectors may not overlap. Since vectors'
--   length is encoded in the type there is no need in runtime
--   checks of length.
copy :: (MVector v a, PrimMonad m)
     => v (PrimState m) a    -- ^ Target
     -> v (PrimState m) a    -- ^ Source
     -> m ()
{-# INLINE copy #-}
copy tgt src = stToPrim $ basicCopy tgt src

-- | Create copy of vector.
--
--   Examples:
--
--   >>> import Control.Monad.ST (runST)
--   >>> import Data.Vector.Fixed (mk3)
--   >>> import Data.Vector.Fixed.Boxed (Vec3)
--   >>> import qualified Data.Vector.Fixed.Mutable as M
--   >>> let x = runST (do { v <- M.replicate 100; v' <- clone v; M.write v' 0 2; M.unsafeFreeze v' }) :: Vec3 Int
--   >>> x
--   [2,100,100]
clone :: (PrimMonad m, MVector v a) => v (PrimState m) a -> m (v (PrimState m) a)
{-# INLINE clone #-}
clone = stToPrim . basicClone

-- | Read value at index without bound checks.
unsafeRead  :: (MVector v a, PrimMonad m) => v (PrimState m) a -> Int -> m a
{-# INLINE unsafeRead #-}
unsafeRead v i = stToPrim $ basicUnsafeRead v i

-- | Write value at index without bound checks.
unsafeWrite :: (MVector v a, PrimMonad m) => v (PrimState m) a -> Int -> a -> m ()
{-# INLINE unsafeWrite #-}
unsafeWrite v i a = stToPrim $ basicUnsafeWrite v i a

-- | Read value at index with bound checks.
read  :: (PrimMonad m, MVector v a) => v (PrimState m) a -> Int -> m a
{-# INLINE read #-}
read v i
  | i < 0 || i >= lengthM v = error "Data.Vector.Fixed.Mutable.read: index out of range"
  | otherwise               = unsafeRead v i

-- | Write value at index with bound checks.
write :: (PrimMonad m, MVector v a) => v (PrimState m) a -> Int -> a -> m ()
{-# INLINE write #-}
write v i x
  | i < 0 || i >= lengthM v = error "Data.Vector.Fixed.Mutable.write: index out of range"
  | otherwise               = unsafeWrite v i x


-- | Create new vector with all elements set to given value.
replicate :: (PrimMonad m, MVector v a) => a -> m (v (PrimState m) a)
{-# INLINE replicate #-}
replicate = stToPrim . basicReplicate

-- | Create new vector with all elements are generated by provided
--   monadic action.
replicateM :: (PrimMonad m, MVector v a) => m a -> m (v (PrimState m) a)
{-# INLINE replicateM #-}
replicateM m = do
  v <- new
  forI v $ \i -> unsafeWrite v i =<< m
  pure v

-- | Create new vector with using function from index to value.
generate :: (PrimMonad m, MVector v a) => (Int -> a) -> m (v (PrimState m) a)
{-# INLINE generate #-}
generate f = do
  v <- new
  forI v $ \i -> unsafeWrite v i $ f i
  pure v

-- | Create new vector with using monadic function from index to value.
generateM :: (PrimMonad m, MVector v a) => (Int -> m a) -> m (v (PrimState m) a)
{-# INLINE generateM #-}
generateM f = do
  v <- new
  forI v $ \i -> unsafeWrite v i =<< f i
  pure v

-- | Loop which calls function for each index
forI :: (PrimMonad m, MVector v a) => v (PrimState m) a -> (Int -> m ()) -> m ()
{-# INLINE forI #-}
forI v f = go 0
  where
    go i | i >= n    = pure ()
         | otherwise = f i >> go (i+1)
    n = lengthM v


----------------------------------------------------------------
-- Immutable
----------------------------------------------------------------

-- | Type class for immutable vectors
class (Dim v ~ DimM (Mutable v), MVector (Mutable v) a) => IVector v a where
  -- | Convert vector to immutable state. Mutable vector must not be
  --   modified afterwards.
  basicUnsafeFreeze :: Mutable v s a -> ST s (v a)
  -- | /O(1)/ Unsafely convert immutable vector to mutable without
  --   copying.  Note that this is a very dangerous function and
  --   generally it's only safe to read from the resulting vector. In
  --   this case, the immutable vector could be used safely as well.
  --
  -- Problems with mutation happen because GHC has a lot of freedom to
  -- introduce sharing. As a result mutable vectors produced by
  -- @unsafeThaw@ may or may not share the same underlying buffer. For
  -- example:
  --
  -- > foo = do
  -- >   let vec = F.generate 10 id
  -- >   mvec <- M.unsafeThaw vec
  -- >   do_something mvec
  --
  -- Here GHC could lift @vec@ outside of foo which means that all calls to
  -- @do_something@ will use same buffer with possibly disastrous
  -- results. Whether such aliasing happens or not depends on the program in
  -- question, optimization levels, and GHC flags.
  --
  -- All in all, attempts to modify a vector produced by @unsafeThaw@
  -- fall out of domain of software engineering and into realm of
  -- black magic, dark rituals, and unspeakable horrors. The only
  -- advice that could be given is: "Don't attempt to mutate a vector
  -- produced by @unsafeThaw@ unless you know how to prevent GHC from
  -- aliasing buffers accidentally. We don't."
  basicUnsafeThaw :: v a -> ST s (Mutable v s a)
  -- | Get element at specified index without bounds check.
  unsafeIndex :: v a -> Int -> a


-- | Convert vector to immutable state. Mutable vector must not be
--   modified afterwards.
unsafeFreeze :: (IVector v a, PrimMonad m) => Mutable v (PrimState m) a -> m (v a)
{-# INLINE unsafeFreeze #-}
unsafeFreeze = stToPrim . basicUnsafeFreeze

-- | /O(1)/ Unsafely convert immutable vector to mutable without
--   copying.  Note that this is a very dangerous function and
--   generally it's only safe to read from the resulting vector. In
--   this case, the immutable vector could be used safely as well.
--
-- Problems with mutation happen because GHC has a lot of freedom to
-- introduce sharing. As a result mutable vectors produced by
-- @unsafeThaw@ may or may not share the same underlying buffer. For
-- example:
--
-- > foo = do
-- >   let vec = F.generate 10 id
-- >   mvec <- M.unsafeThaw vec
-- >   do_something mvec
--
-- Here GHC could lift @vec@ outside of foo which means that all calls to
-- @do_something@ will use same buffer with possibly disastrous
-- results. Whether such aliasing happens or not depends on the program in
-- question, optimization levels, and GHC flags.
--
-- All in all, attempts to modify a vector produced by @unsafeThaw@
-- fall out of domain of software engineering and into realm of
-- black magic, dark rituals, and unspeakable horrors. The only
-- advice that could be given is: "Don't attempt to mutate a vector
-- produced by @unsafeThaw@ unless you know how to prevent GHC from
-- aliasing buffers accidentally. We don't."
unsafeThaw :: PrimMonad m => v a -> m (Mutable v (PrimState m) a)
{-# INLINE unsafeThaw #-}
unsafeThaw = stToPrim . unsafeThaw


index :: IVector v a => v a -> Int -> a
{-# INLINE index #-}
index v i | i < 0 || i >= length v = error "Data.Vector.Fixed.Mutable.!: index out of bounds"
          | otherwise              = unsafeIndex v i


-- | Safely convert mutable vector to immutable.
freeze :: (PrimMonad m, IVector v a) => Mutable v (PrimState m) a -> m (v a)
{-# INLINE freeze #-}
freeze v = unsafeFreeze =<< clone v

-- | Safely convert immutable vector to mutable.
thaw :: (PrimMonad m, IVector v a) => v a -> m (Mutable v (PrimState m) a)
{-# INLINE thaw #-}
thaw v = clone =<< unsafeThaw v



----------------------------------------------------------------
-- Vector API
----------------------------------------------------------------

-- | Generic inspect implementation for array-based vectors.
inspectVec :: forall v a b. (ArityPeano (Dim v), IVector v a) => v a -> Fun (Dim v) a b -> b
{-# INLINE inspectVec #-}
inspectVec v
  = inspect cv
  where
    cv :: ContVec (Dim v) a
    cv = apply (\(Const i) -> (unsafeIndex v i, Const (i+1)))
               (Const 0 :: Const Int (Dim v))

-- | Generic construct implementation for array-based vectors.
constructVec :: forall v a. (ArityPeano (Dim v), IVector v a) => Fun (Dim v) a (v a)
{-# INLINE constructVec #-}
constructVec =
  accum step
        (\(T_new _ st) -> runST $ unsafeFreeze =<< st :: v a)
        (T_new 0 new :: T_new v a (Dim v))

data T_new v a n = T_new Int (forall s. ST s (Mutable v s a))

step :: (IVector v a) => T_new v a ('S n) -> a -> T_new v a n
step (T_new i st) x = T_new (i+1) $ do
  mv <- st
  unsafeWrite mv i x
  return mv
