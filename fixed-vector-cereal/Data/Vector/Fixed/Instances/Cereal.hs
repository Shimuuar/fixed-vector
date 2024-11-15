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
import qualified Data.Vector.Fixed.Boxed     as B
import qualified Data.Vector.Fixed.Unboxed   as U
import qualified Data.Vector.Fixed.Primitive as P
import qualified Data.Vector.Fixed.Storable  as S
import           Data.Serialize                (Serialize(..))


instance (Vector v a, Serialize a) => Serialize (ViaFixed v a) where
  put = F.mapM_ put
  get = F.replicateM get
  {-# INLINE put #-}
  {-# INLINE get #-}

deriving via ViaFixed (B.Vec n) a instance (Arity n, Serialize a)               => Serialize (B.Vec n a)
deriving via ViaFixed (P.Vec n) a instance (Arity n, Serialize a, P.Prim a)     => Serialize (P.Vec n a)
deriving via ViaFixed (S.Vec n) a instance (Arity n, Serialize a, S.Storable a) => Serialize (S.Vec n a)
deriving via ViaFixed (U.Vec n) a instance (Arity n, Serialize a, U.Unbox n a)  => Serialize (U.Vec n a)

deriving via ViaFixed (F.VecList  n) a instance (Arity n,      Serialize a) => Serialize (F.VecList  n a)
deriving via ViaFixed (F.VecPeano n) a instance (ArityPeano n, Serialize a) => Serialize (F.VecPeano n a)

instance (Serialize a) => Serialize (F.Only a) where
  put (F.Only a) = put a
  get = F.Only `fmap` get

instance Serialize (F.Empty a) where
  put _ = return ()
  get = return F.Empty
