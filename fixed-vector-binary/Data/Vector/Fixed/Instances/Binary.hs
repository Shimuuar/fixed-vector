{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Module with binary instances for data types defined in fixed
--   vector
module Data.Vector.Fixed.Instances.Binary where

import           Data.Vector.Fixed             (Arity,ArityPeano,ViaFixed(..),Vector)
import qualified Data.Vector.Fixed           as F
import qualified Data.Vector.Fixed.Boxed     as FB
import qualified Data.Vector.Fixed.Strict    as FF
import qualified Data.Vector.Fixed.Unboxed   as FU
import qualified Data.Vector.Fixed.Primitive as FP
import qualified Data.Vector.Fixed.Storable  as FS
import           Data.Binary                   (Binary(..))

instance (Vector v a, Binary a) => Binary (ViaFixed v a) where
  put = F.mapM_ put
  get = F.replicateM get
  {-# INLINE put #-}
  {-# INLINE get #-}

deriving via ViaFixed (FB.Vec n) a instance (Arity n, Binary a)                => Binary (FB.Vec n a)
deriving via ViaFixed (FF.Vec n) a instance (Arity n, Binary a)                => Binary (FF.Vec n a)
deriving via ViaFixed (FP.Vec n) a instance (Arity n, Binary a, FP.Prim a)     => Binary (FP.Vec n a)
deriving via ViaFixed (FS.Vec n) a instance (Arity n, Binary a, FS.Storable a) => Binary (FS.Vec n a)
deriving via ViaFixed (FU.Vec n) a instance (Arity n, Binary a, FU.Unbox n a)  => Binary (FU.Vec n a)

deriving via ViaFixed (F.VecList  n) a instance (Arity n,      Binary a) => Binary (F.VecList  n a)
deriving via ViaFixed (F.VecPeano n) a instance (ArityPeano n, Binary a) => Binary (F.VecPeano n a)

instance (Binary a) => Binary (F.Only a) where
  put (F.Only a) = put a
  get = F.Only `fmap` get

instance Binary (F.Empty a) where
  put _ = return ()
  get = return F.Empty
