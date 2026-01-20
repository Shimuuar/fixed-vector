{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Module with cereal instances for data types defined in fixed
--   vector
module Data.Vector.Fixed.Instances.Cereal where

import           Data.Vector.Fixed             (Arity,ArityPeano,ViaFixed(..),Vector)
import qualified Data.Vector.Fixed           as F
import qualified Data.Vector.Fixed.Boxed     as FB
import qualified Data.Vector.Fixed.Strict    as FF
import qualified Data.Vector.Fixed.Unboxed   as FU
import qualified Data.Vector.Fixed.Primitive as FP
import qualified Data.Vector.Fixed.Storable  as FS
import qualified Data.Vector.Fixed.Mono      as FM
import           Data.Serialize                (Serialize(..))


instance (Vector v a, Serialize a) => Serialize (ViaFixed v a) where
  put = F.mapM_ put
  get = F.replicateM get
  {-# INLINE put #-}
  {-# INLINE get #-}

instance (FM.Prod a v, Serialize a) => Serialize (FM.ViaFixed a v) where
  put = FM.mapM_ put
  get = FM.replicateM get
  {-# INLINE put #-}
  {-# INLINE get #-}

deriving via ViaFixed (FB.Vec n) a instance (Arity n, Serialize a)                => Serialize (FB.Vec n a)
deriving via ViaFixed (FF.Vec n) a instance (Arity n, Serialize a)                => Serialize (FF.Vec n a)
deriving via ViaFixed (FP.Vec n) a instance (Arity n, Serialize a, FP.Prim a)     => Serialize (FP.Vec n a)
deriving via ViaFixed (FS.Vec n) a instance (Arity n, Serialize a, FS.Storable a) => Serialize (FS.Vec n a)
deriving via ViaFixed (FU.Vec n) a instance (Arity n, Serialize a, FU.Unbox n a)  => Serialize (FU.Vec n a)

deriving via ViaFixed (F.VecList  n) a instance (Arity n,      Serialize a) => Serialize (F.VecList  n a)
deriving via ViaFixed (F.VecPeano n) a instance (ArityPeano n, Serialize a) => Serialize (F.VecPeano n a)

instance (Serialize a) => Serialize (F.Only a) where
  put (F.Only a) = put a
  get = F.Only `fmap` get

instance Serialize (F.Empty a) where
  put _ = return ()
  get = return F.Empty
