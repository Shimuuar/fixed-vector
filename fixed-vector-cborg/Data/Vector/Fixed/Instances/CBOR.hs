{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE MagicHash            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
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

import           Data.Vector.Fixed             (Arity)
import qualified Data.Vector.Fixed           as F
import           Data.Vector.Fixed.Cont        (peanoToInt,Dim)
import qualified Data.Vector.Fixed.Boxed     as B
import qualified Data.Vector.Fixed.Unboxed   as U
import qualified Data.Vector.Fixed.Primitive as P
import qualified Data.Vector.Fixed.Storable  as S



instance (Arity n, Serialise a) => Serialise (B.Vec n a) where
  encode = encodeFixedVector
  decode = decodeFixedVector

instance (Arity n, P.Prim a, Serialise a) => Serialise (P.Vec n a) where
  encode = encodeFixedVector
  decode = decodeFixedVector

instance (Arity n, S.Storable a, Serialise a) => Serialise (S.Vec n a) where
  encode = encodeFixedVector
  decode = decodeFixedVector

instance (U.Unbox n a, Serialise a) => Serialise (U.Vec n a) where
  encode = encodeFixedVector
  decode = decodeFixedVector

instance (Arity n, Serialise a) => Serialise (F.VecList n a) where
  encode = encodeFixedVector
  decode = decodeFixedVector

instance (Serialise a) => Serialise (F.Only a) where
  encode = encodeFixedVector
  decode = decodeFixedVector

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
