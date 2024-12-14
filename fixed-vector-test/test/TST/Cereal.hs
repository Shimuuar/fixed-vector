{-# LANGUAGE TemplateHaskell #-}
module TST.Cereal (tests) where

import Data.Serialize
import Data.Vector.Fixed.Instances.QuickCheck ()
import Data.Vector.Fixed.Instances.Cereal     ()
import TST.Util

tests :: TestTree
tests
  = testGroup "cereal"
  $ $(makeTest 'testCereal [t| Int |])

testCereal
  :: forall v a. ( Typeable a, Typeable v
                 , Arbitrary (v a), Eq (v a), Show (v a), Serialize (v a)
                 )
  => Proxy v
  -> Proxy a
  -> TestTree
testCereal _ _
  = testProperty (show $ typeOf (undefined :: v a))
  $ \(v :: v a) -> Right v == (decode . encode) v


