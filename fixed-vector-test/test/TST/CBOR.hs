{-# LANGUAGE TemplateHaskell #-}
module TST.CBOR where

import Codec.Serialise
import Data.Vector.Fixed.Instances.QuickCheck ()
import Data.Vector.Fixed.Instances.CBOR       ()
import TST.Util

tests :: TestTree
tests
  = testGroup "CBOR"
  $ $(makeTest 'testCBOR [t| Int |])

testCBOR
  :: forall v a. ( Typeable a, Typeable v
                 , Arbitrary (v a), Eq (v a), Show (v a), Serialise (v a)
                 )
  => Proxy v
  -> Proxy a
  -> TestTree
testCBOR _ _
  = testProperty (show $ typeOf (undefined :: v a))
  $ \(v :: v a) -> v == (deserialise . serialise) v
