{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Data.Typeable
import Data.Serialize
import qualified Data.Vector.Fixed           as F
import qualified Data.Vector.Fixed.Unboxed   as U
import qualified Data.Vector.Fixed.Boxed     as B
import qualified Data.Vector.Fixed.Storable  as S
import qualified Data.Vector.Fixed.Primitive as P
import           Data.Vector.Fixed.Instances.Cereal ()



tests = testGroup "cereal"
  [ testTagged p_serialize (T :: T (F.Only  Int))
  , testTagged p_serialize (T :: T (F.VecList 2 Int))
  , testTagged p_serialize (T :: T (F.VecList 3 Int))
  , testTagged p_serialize (T :: T (F.VecList 4 Int))
    --
  , testTagged p_serialize (T :: T (U.Vec 2 Int))
  , testTagged p_serialize (T :: T (U.Vec 3 Int))
  , testTagged p_serialize (T :: T (U.Vec 4 Int))
    --
  , testTagged p_serialize (T :: T (B.Vec 2 Int))
  , testTagged p_serialize (T :: T (B.Vec 3 Int))
  , testTagged p_serialize (T :: T (B.Vec 4 Int))
    --
  , testTagged p_serialize (T :: T (S.Vec 2 Int))
  , testTagged p_serialize (T :: T (S.Vec 3 Int))
  , testTagged p_serialize (T :: T (S.Vec 4 Int))
    --
  , testTagged p_serialize (T :: T (P.Vec 2 Int))
  , testTagged p_serialize (T :: T (P.Vec 3 Int))
  , testTagged p_serialize (T :: T (P.Vec 4 Int))
  ]

p_serialize :: (Serialize a, Arbitrary a, Eq a) => T a -> a -> Bool
p_serialize _ a = Right a == (decode . encode) a

data T a = T

testTagged :: forall a b. (Testable b, Typeable a) => (T a -> b) -> T a -> TestTree
testTagged prop t
  = testProperty (show $ typeOf (undefined :: a)) (prop t)

main :: IO ()
main = defaultMain tests

----------------------------------------------------------------
instance Arbitrary a => Arbitrary (F.Only a) where
  arbitrary = F.replicateM arbitrary
instance Arbitrary a => Arbitrary (F.Empty a) where
  arbitrary = F.replicateM arbitrary

instance (F.Arity n, Arbitrary a) => Arbitrary (F.VecList n a) where
  arbitrary = F.replicateM arbitrary
instance (U.Unbox n a, Arbitrary a) => Arbitrary (U.Vec n a) where
  arbitrary = F.replicateM arbitrary
instance (F.Arity n, Arbitrary a) => Arbitrary (B.Vec n a) where
  arbitrary = F.replicateM arbitrary
instance (F.Arity n, S.Storable a, Arbitrary a) => Arbitrary (S.Vec n a) where
  arbitrary = F.replicateM arbitrary
instance (F.Arity n, P.Prim a, Arbitrary a) => Arbitrary (P.Vec n a) where
  arbitrary = F.replicateM arbitrary



