{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}
module TST.Util
  ( makeTest
    -- * Reexports
  , TestTree
  , Arbitrary
  , testGroup
  , testProperty
  , Proxy(..)
  , Typeable
  , typeOf
  ) where

import Test.Tasty
import Test.Tasty.QuickCheck

import Data.Typeable
import qualified Data.Vector.Fixed           as F
import qualified Data.Vector.Fixed.Unboxed   as FU
import qualified Data.Vector.Fixed.Boxed     as FB
import qualified Data.Vector.Fixed.Strict    as FF
import qualified Data.Vector.Fixed.Storable  as FS
import qualified Data.Vector.Fixed.Primitive as FP

import Language.Haskell.TH


-- | Use template haskell to generate all test cases
makeTest
  :: Name  -- ^ Name of function for generating tests
           --   Its type should be @Proxy v → Proxy a → TestTree@.
  -> TypeQ -- ^ Type of element to use
  -> ExpQ
makeTest (varE -> test) ty = [|
  [ $(test) (Proxy @F.Empty) (Proxy :: Proxy $(ty))
  , $(test) (Proxy @F.Only)  (Proxy :: Proxy $(ty))
    --
  , $(test) (Proxy @(F.VecList 0)) (Proxy :: Proxy $(ty))
  , $(test) (Proxy @(F.VecList 1)) (Proxy :: Proxy $(ty))
  , $(test) (Proxy @(F.VecList 2)) (Proxy :: Proxy $(ty))
  , $(test) (Proxy @(F.VecList 3)) (Proxy :: Proxy $(ty))
    --
  , $(test) (Proxy @(FU.Vec 0)) (Proxy :: Proxy $(ty))
  , $(test) (Proxy @(FU.Vec 1)) (Proxy :: Proxy $(ty))
  , $(test) (Proxy @(FU.Vec 2)) (Proxy :: Proxy $(ty))
  , $(test) (Proxy @(FU.Vec 3)) (Proxy :: Proxy $(ty))
    --
  , $(test) (Proxy @(FB.Vec 0)) (Proxy :: Proxy $(ty))
  , $(test) (Proxy @(FB.Vec 1)) (Proxy :: Proxy $(ty))
  , $(test) (Proxy @(FB.Vec 2)) (Proxy :: Proxy $(ty))
  , $(test) (Proxy @(FB.Vec 3)) (Proxy :: Proxy $(ty))
    --
  , $(test) (Proxy @(FF.Vec 0)) (Proxy :: Proxy $(ty))
  , $(test) (Proxy @(FF.Vec 1)) (Proxy :: Proxy $(ty))
  , $(test) (Proxy @(FF.Vec 2)) (Proxy :: Proxy $(ty))
  , $(test) (Proxy @(FF.Vec 3)) (Proxy :: Proxy $(ty))
    --
  , $(test) (Proxy @(FS.Vec 0)) (Proxy :: Proxy $(ty))
  , $(test) (Proxy @(FS.Vec 1)) (Proxy :: Proxy $(ty))
  , $(test) (Proxy @(FS.Vec 2)) (Proxy :: Proxy $(ty))
  , $(test) (Proxy @(FS.Vec 3)) (Proxy :: Proxy $(ty))
    --
  , $(test) (Proxy @(FP.Vec 0)) (Proxy :: Proxy $(ty))
  , $(test) (Proxy @(FP.Vec 1)) (Proxy :: Proxy $(ty))
  , $(test) (Proxy @(FP.Vec 2)) (Proxy :: Proxy $(ty))
  , $(test) (Proxy @(FP.Vec 3)) (Proxy :: Proxy $(ty))
  ]
  |]
