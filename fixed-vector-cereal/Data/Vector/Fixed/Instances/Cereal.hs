{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Module with cereal instances for data types defined in fixed
--   vector
module Data.Vector.Fixed.Instances.Cereal where

import           Data.Vector.Fixed             (Arity)
import qualified Data.Vector.Fixed           as F
import qualified Data.Vector.Fixed.Boxed     as B
import qualified Data.Vector.Fixed.Unboxed   as U
import qualified Data.Vector.Fixed.Primitive as P
import qualified Data.Vector.Fixed.Storable  as S
import           Data.Serialize                (Serialize(..))


instance (Arity n, Serialize a) => Serialize (B.Vec n a) where
  put = F.mapM_ put
  get = F.replicateM get

instance (Arity n, P.Prim a, Serialize a) => Serialize (P.Vec n a) where
  put = F.mapM_ put
  get = F.replicateM get

instance (Arity n, S.Storable a, Serialize a) => Serialize (S.Vec n a) where
  put = F.mapM_ put
  get = F.replicateM get

instance (U.Unbox n a, Serialize a) => Serialize (U.Vec n a) where
  put = F.mapM_ put
  get = F.replicateM get

instance (Arity n, Serialize a) => Serialize (F.VecList n a) where
  put = F.mapM_ put
  get = F.replicateM get

instance (Serialize a) => Serialize (F.Only a) where
  put (F.Only a) = put a
  get = F.Only `fmap` get

instance Serialize (F.Empty a) where
  put _ = return ()
  get = return F.Empty
