{-# LANGUAGE TemplateHaskell     #-}
module TST.Aeson where

import Data.Aeson
import Data.Vector.Fixed.Instances.QuickCheck ()
import Data.Vector.Fixed.Instances.Aeson      ()
import TST.Util

tests :: TestTree
tests
  = testGroup "aeson"
  $ $(makeTest 'testAeson [t| Int |])

testAeson
  :: forall v a. ( Typeable a, Typeable v
                 , Arbitrary (v a), Eq (v a), Show (v a), FromJSON (v a), ToJSON (v a)
                 )
  => Proxy v
  -> Proxy a
  -> TestTree
testAeson _ _
  = testProperty (show $ typeOf (undefined :: v a))
  $ \(v :: v a) -> Just v == (decode . encode) v
