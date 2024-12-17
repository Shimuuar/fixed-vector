{-# LANGUAGE MagicHash       #-}
{-# LANGUAGE TemplateHaskell #-}
-- |
module Inspect.Obligations where

import GHC.Exts
import Test.Tasty.Inspection
import Language.Haskell.TH (Name)


-- We don't allocate arrays in he function. It covers opaque data
-- types
noArrayAlloc :: Name -> Obligation
noArrayAlloc nm = doesNotUseAnyOf nm
  [ 'newByteArray#
  , 'newSmallArray# 
  ]

noAllocation :: Name -> Obligation
noAllocation nm = mkObligation nm NoAllocation
