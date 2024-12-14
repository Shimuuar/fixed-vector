{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Module with @aeson@ instances for data types defined in fixed
--   vector
module Data.Vector.Fixed.Instances.Aeson
  ( fixedVectorParseJSON
  , fixedVectorToJSON
  , fixedVectorToEncoding
  ) where

import Control.Monad
import Control.Monad.ST
import           Data.Vector.Fixed             (Arity,ArityPeano,ViaFixed(..),Vector)
import qualified Data.Vector.Fixed           as F
import qualified Data.Vector.Fixed.Boxed     as FB
import qualified Data.Vector.Fixed.Strict    as FF
import qualified Data.Vector.Fixed.Unboxed   as FU
import qualified Data.Vector.Fixed.Primitive as FP
import qualified Data.Vector.Fixed.Storable  as FS
import           Data.Aeson
import           Data.Aeson.Types

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV


----------------------------------------------------------------
-- Generic implementations
----------------------------------------------------------------

-- | Generic implementation of 'parseJSON' for data types which are
--   instances of 'Vector'.
fixedVectorParseJSON :: forall v a. (Vector v a, FromJSON a) => Value -> Parser (v a)
{-# INLINE fixedVectorParseJSON #-}
fixedVectorParseJSON = withArray "fixed-vector" $ \arr -> do
  let expected = F.length (undefined :: v a)
  when (V.length arr /= expected) $
    fail $ "Expecting array of length " ++ show expected
  F.generateM $ \i -> parseJSON (arr V.! i)

-- | Generic implementation of 'toJSON' for data types which are
--   instances of 'Vector'.
fixedVectorToJSON :: forall v a. (Vector v a, ToJSON a) => v a -> Value
{-# INLINE fixedVectorToJSON #-}
fixedVectorToJSON v = Array $ runST $ do
  -- NOTE: (!) from fixed vector could have O(n) complexity so let
  --       fold over fixed vector. Access to vector _is_ O(1)
  vec <- MV.unsafeNew n
  flip F.imapM_ v $ \i a -> MV.unsafeWrite vec i (toJSON a)
  V.unsafeFreeze vec
  where
    n = F.length v

-- | Generic implementation of 'toEncoding' for data types which are
--   instances of 'Vector'.
fixedVectorToEncoding :: forall v a. (Vector v a, ToJSON a) => v a -> Encoding
{-# INLINE fixedVectorToEncoding #-}
fixedVectorToEncoding = foldable . F.cvec


----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

instance (Vector v a, FromJSON a) => FromJSON (ViaFixed v a) where
  {-# INLINE parseJSON #-}
  parseJSON = fixedVectorParseJSON

instance (Vector v a, ToJSON a) => ToJSON (ViaFixed v a) where
  toJSON     = fixedVectorToJSON
  toEncoding = fixedVectorToEncoding
  {-# INLINE toJSON     #-}
  {-# INLINE toEncoding #-}



deriving via ViaFixed (FB.Vec n) a instance (Arity n, FromJSON a)                => FromJSON (FB.Vec n a)
deriving via ViaFixed (FB.Vec n) a instance (Arity n, ToJSON   a)                => ToJSON   (FB.Vec n a)
deriving via ViaFixed (FF.Vec n) a instance (Arity n, FromJSON a)                => FromJSON (FF.Vec n a)
deriving via ViaFixed (FF.Vec n) a instance (Arity n, ToJSON   a)                => ToJSON   (FF.Vec n a)
deriving via ViaFixed (FP.Vec n) a instance (Arity n, FromJSON a, FP.Prim a)     => FromJSON (FP.Vec n a)
deriving via ViaFixed (FP.Vec n) a instance (Arity n, ToJSON   a, FP.Prim a)     => ToJSON   (FP.Vec n a)
deriving via ViaFixed (FS.Vec n) a instance (Arity n, FromJSON a, FS.Storable a) => FromJSON (FS.Vec n a)
deriving via ViaFixed (FS.Vec n) a instance (Arity n, ToJSON   a, FS.Storable a) => ToJSON   (FS.Vec n a)
deriving via ViaFixed (FU.Vec n) a instance (Arity n, FromJSON a, FU.Unbox n a)  => FromJSON (FU.Vec n a)
deriving via ViaFixed (FU.Vec n) a instance (Arity n, ToJSON   a, FU.Unbox n a)  => ToJSON   (FU.Vec n a)

deriving via ViaFixed (F.VecList  n) a instance (Arity n,      FromJSON a) => FromJSON (F.VecList  n a)
deriving via ViaFixed (F.VecList  n) a instance (Arity n,      ToJSON   a) => ToJSON   (F.VecList  n a)
deriving via ViaFixed (F.VecPeano n) a instance (ArityPeano n, FromJSON a) => FromJSON (F.VecPeano n a)
deriving via ViaFixed (F.VecPeano n) a instance (ArityPeano n, ToJSON   a) => ToJSON   (F.VecPeano n a)

deriving via ViaFixed F.Only a instance (FromJSON a) => FromJSON (F.Only a)
deriving via ViaFixed F.Only a instance (ToJSON   a) => ToJSON   (F.Only a)

instance FromJSON (F.Empty a) where
  parseJSON = withArray "fixed-vector: Empty" $ \arr -> do
    unless (V.null arr) $ fail "Nonempty array"
    pure F.Empty
instance ToJSON (F.Empty a) where
  toJSON     _ = Array V.empty
  toEncoding _ = toEncoding ([]::[Value])
