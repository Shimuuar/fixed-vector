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
import qualified Data.Vector.Fixed.Boxed     as B
import qualified Data.Vector.Fixed.Unboxed   as U
import qualified Data.Vector.Fixed.Primitive as P
import qualified Data.Vector.Fixed.Storable  as S
import           Data.Binary                   (Binary(..))

instance (Vector v a, Binary a) => Binary (ViaFixed v a) where
  put = F.mapM_ put
  get = F.replicateM get
  {-# INLINE put #-}
  {-# INLINE get #-}

deriving via ViaFixed (B.Vec n) a instance (Arity n, Binary a)               => Binary (B.Vec n a)
deriving via ViaFixed (P.Vec n) a instance (Arity n, Binary a, P.Prim a)     => Binary (P.Vec n a)
deriving via ViaFixed (S.Vec n) a instance (Arity n, Binary a, S.Storable a) => Binary (S.Vec n a)
deriving via ViaFixed (U.Vec n) a instance (Arity n, Binary a, U.Unbox n a)  => Binary (U.Vec n a)

deriving via ViaFixed (F.VecList  n) a instance (Arity n,      Binary a) => Binary (F.VecList  n a)
deriving via ViaFixed (F.VecPeano n) a instance (ArityPeano n, Binary a) => Binary (F.VecPeano n a)

instance (Binary a) => Binary (F.Only a) where
  put (F.Only a) = put a
  get = F.Only `fmap` get

instance Binary (F.Empty a) where
  put _ = return ()
  get = return F.Empty
