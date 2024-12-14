{-# LANGUAGE TemplateHaskell #-}
module TST.Binary (tests) where

import Data.Binary
import Data.Vector.Fixed.Instances.QuickCheck ()
import Data.Vector.Fixed.Instances.Binary     ()
import TST.Util

tests :: TestTree
tests
  = testGroup "binary"
  $ $(makeTest 'testBinary [t| Int |])

testBinary
  :: forall v a. ( Typeable a, Typeable v
                 , Arbitrary (v a), Eq (v a), Show (v a), Binary (v a)
                 )
  => Proxy v
  -> Proxy a
  -> TestTree
testBinary _ _
  = testProperty (show $ typeOf (undefined :: v a))
  $ \(v :: v a) -> v == (decode . encode) v
