{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Module with @aeson@ instances for data types defined in fixed
--   vector
module Data.Vector.Fixed.Instances.QuickCheck () where

import           Data.Vector.Fixed             (Arity,ArityPeano,ViaFixed(..),Vector)
import qualified Data.Vector.Fixed           as F
import qualified Data.Vector.Fixed.Boxed     as FB
import qualified Data.Vector.Fixed.Strict    as FF
import qualified Data.Vector.Fixed.Unboxed   as FU
import qualified Data.Vector.Fixed.Primitive as FP
import qualified Data.Vector.Fixed.Storable  as FS
import qualified Data.Vector.Fixed.Mono      as FM
import           Test.QuickCheck


instance (Vector v a, Arbitrary a) => Arbitrary (ViaFixed v a) where
  arbitrary = F.replicateM arbitrary
instance (FM.Prod a v, Arbitrary a) => Arbitrary (FM.ViaFixed a v) where
  arbitrary = FM.replicateM arbitrary


deriving via ViaFixed (FB.Vec n) a instance (Arity n, Arbitrary a)                => Arbitrary (FB.Vec n a)
deriving via ViaFixed (FF.Vec n) a instance (Arity n, Arbitrary a)                => Arbitrary (FF.Vec n a)
deriving via ViaFixed (FP.Vec n) a instance (Arity n, Arbitrary a, FP.Prim a)     => Arbitrary (FP.Vec n a)
deriving via ViaFixed (FS.Vec n) a instance (Arity n, Arbitrary a, FS.Storable a) => Arbitrary (FS.Vec n a)
deriving via ViaFixed (FU.Vec n) a instance (Arity n, Arbitrary a, FU.Unbox n a)  => Arbitrary (FU.Vec n a)

deriving via ViaFixed (F.VecList  n) a instance (Arity n,      Arbitrary a) => Arbitrary (F.VecList  n a)
deriving via ViaFixed (F.VecPeano n) a instance (ArityPeano n, Arbitrary a) => Arbitrary (F.VecPeano n a)
deriving via ViaFixed F.Only         a instance (Arbitrary a)               => Arbitrary (F.Only a)

instance Arbitrary (F.Empty a) where
  arbitrary = pure F.Empty
