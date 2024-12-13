{-# LANGUAGE DerivingVia          #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | Module with binary instances for data types defined in fixed
--   vector
module Data.Vector.Fixed.Instances.CBOR where

import           Codec.Serialise
import           Codec.CBOR.Encoding           (Encoding,encodeListLen,encodeNull)
import           Codec.CBOR.Decoding           (Decoder,decodeListLenOf,decodeNull)
import           GHC.Exts                      (proxy#)

import           Data.Vector.Fixed             (Arity,ArityPeano,Vector,ViaFixed)
import qualified Data.Vector.Fixed           as F
import           Data.Vector.Fixed.Cont        (peanoToInt,Dim)
import qualified Data.Vector.Fixed.Boxed     as FB
import qualified Data.Vector.Fixed.Strict    as FF
import qualified Data.Vector.Fixed.Unboxed   as FU
import qualified Data.Vector.Fixed.Primitive as FP
import qualified Data.Vector.Fixed.Storable  as FS


instance (Vector v a, Serialise a) => Serialise (ViaFixed v a) where
  encode = encodeFixedVector
  decode = decodeFixedVector
  {-# INLINE encode #-}
  {-# INLINE decode #-}

deriving via ViaFixed (FB.Vec n) a instance (Arity n, Serialise a)                => Serialise (FB.Vec n a)
deriving via ViaFixed (FF.Vec n) a instance (Arity n, Serialise a)                => Serialise (FF.Vec n a)
deriving via ViaFixed (FP.Vec n) a instance (Arity n, Serialise a, FP.Prim a)     => Serialise (FP.Vec n a)
deriving via ViaFixed (FS.Vec n) a instance (Arity n, Serialise a, FS.Storable a) => Serialise (FS.Vec n a)
deriving via ViaFixed (FU.Vec n) a instance (Arity n, Serialise a, FU.Unbox n a)  => Serialise (FU.Vec n a)

deriving via ViaFixed (F.VecList  n) a instance (Arity n,      Serialise a) => Serialise (F.VecList  n a)
deriving via ViaFixed (F.VecPeano n) a instance (ArityPeano n, Serialise a) => Serialise (F.VecPeano n a)

deriving via ViaFixed F.Only a instance (Serialise a) => Serialise (F.Only a)

instance Serialise (F.Empty a) where
  encode = const encodeNull
  decode = F.Empty <$ decodeNull

-- | Encode vector with statically known size as CBOR list. There's no
--   type tag
encodeFixedVector :: (F.Vector v a, Serialise a) => v a -> Encoding
{-# INLINE encodeFixedVector #-}
encodeFixedVector v = encodeListLen (fromIntegral $ F.length v)
                   <> F.foldMap encode v

-- | Decode vector with statically known size as CBOR list. There's no
--   type tag
decodeFixedVector :: forall v s a. (F.Vector v a, Serialise a) => Decoder s (v a)
{-# INLINE decodeFixedVector #-}
decodeFixedVector = do
  decodeListLenOf (fromIntegral $ peanoToInt (proxy# @(Dim v)))
  F.replicateM decode
