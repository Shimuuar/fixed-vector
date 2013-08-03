{-# LANGUAGE TypeFamilies #-}
-- |
-- More generic version of function from "Data.Vector.Fixed"
-- module. They do not require that all vector have same type, only
-- same length. All such functions have suffix /G/.
module Data.Vector.Fixed.Generic (
    -- * Mapping
    mapG
  , imapG
  , mapMG
  , imapMG
    -- * Zips
  , zipWithG
  , izipWithG
  , zipWithMG
  , izipWithMG
  ) where

import Control.Monad (liftM)
import           Data.Vector.Fixed.Cont (Vector,Dim)
import qualified Data.Vector.Fixed.Cont as C



-- | Map over vector
mapG :: (Vector v a, Vector w b, Dim v ~ Dim w)
     => (a -> b) -> v a -> w b
{-# INLINE mapG #-}
mapG f = C.vector
       . C.map f
       . C.cvec

-- | Apply function to every element of the vector and its index.
imapG :: (Vector v a, Vector w b, Dim v ~ Dim w)
      => (Int -> a -> b) -> v a -> w b
{-# INLINE imapG #-}
imapG f = C.vector
        . C.imap f
        . C.cvec

-- | Monadic map over vector.
mapMG :: (Vector v a, Vector w b, Dim w ~ Dim v, Monad m)
      => (a -> m b) -> v a -> m (w b)
{-# INLINE mapMG #-}
mapMG f = liftM C.vector
       . C.mapM f
       . C.cvec

-- | Monadic map over vector.
imapMG :: (Vector v a, Vector w b, Dim w ~ Dim v, Monad m)
       => (Int -> a -> m b) -> v a -> m (w b)
{-# INLINE imapMG #-}
imapMG f = liftM C.vector
         . C.imapM f
         . C.cvec


-- | Zip two vector together using function.
zipWithG :: (Vector v a, Vector w b, Vector u c, Dim v ~ Dim u, Dim v ~ Dim w)
         => (a -> b -> c) -> v a -> w b -> u c
{-# INLINE zipWithG #-}
zipWithG f v u = C.vector
               $ C.zipWith f (C.cvec v) (C.cvec u)

-- | Zip two vector together using monadic function.
zipWithMG :: (Vector v a, Vector w b, Vector u c, Dim v ~ Dim u, Dim v ~ Dim w, Monad m)
          => (a -> b -> m c) -> v a -> w b -> m (u c)
{-# INLINE zipWithMG #-}
zipWithMG f v u = liftM C.vector
                $ C.zipWithM f (C.cvec v) (C.cvec u)

-- | Zip two vector together using function which takes element index
--   as well.
izipWithG :: (Vector v a, Vector w b, Vector u c, Dim v ~ Dim u, Dim v ~ Dim w)
          => (Int -> a -> b -> c) -> v a -> w b -> u c
{-# INLINE izipWithG #-}
izipWithG f v u = C.vector
                $ C.izipWith f (C.cvec v) (C.cvec u)

-- | Zip two vector together using monadic function which takes element
--   index as well..
izipWithMG :: (Vector v a, Vector w b, Vector u c, Dim v ~ Dim u, Dim v ~ Dim w, Monad m)
           => (Int -> a -> b -> m c) -> v a -> w b -> m (u c)
{-# INLINE izipWithMG #-}
izipWithMG f v u = liftM C.vector
                 $ C.izipWithM f (C.cvec v) (C.cvec u)
