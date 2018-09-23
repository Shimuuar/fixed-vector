{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Module with binary instances for data types defined in fixed
--   vector
module Data.Vector.Fixed.Instances.Binary where

import           Data.Vector.Fixed             (Arity)
import qualified Data.Vector.Fixed           as F
import qualified Data.Vector.Fixed.Boxed     as B
import qualified Data.Vector.Fixed.Unboxed   as U
import qualified Data.Vector.Fixed.Primitive as P
import qualified Data.Vector.Fixed.Storable  as S
import           Data.Binary                   (Binary(..))


instance (Arity n, Binary a) => Binary (B.Vec n a) where
  put = F.mapM_ put
  get = F.replicateM get

instance (Arity n, P.Prim a, Binary a) => Binary (P.Vec n a) where
  put = F.mapM_ put
  get = F.replicateM get

instance (Arity n, S.Storable a, Binary a) => Binary (S.Vec n a) where
  put = F.mapM_ put
  get = F.replicateM get

instance (U.Unbox n a, Binary a) => Binary (U.Vec n a) where
  put = F.mapM_ put
  get = F.replicateM get

instance (Arity n, Binary a) => Binary (F.VecList n a) where
  put = F.mapM_ put
  get = F.replicateM get

instance (Binary a) => Binary (F.Only a) where
  put (F.Only a) = put a
  get = F.Only `fmap` get

instance Binary (F.Empty a) where
  put _ = return ()
  get = return F.Empty
