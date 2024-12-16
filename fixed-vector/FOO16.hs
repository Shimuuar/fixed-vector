-- |
{-# OPTIONS_GHC -ddump-timings -ddump-to-file #-}
module FOO16 where

import Data.Vector.Fixed qualified as F
import Data.Vector.Fixed.Primitive (Vec)

type N = 16

foo :: Vec N Int -> Vec N Int -> Vec N Int -> Vec N Int -> Int
foo v1 v2 v3 v4
  = F.sum v12
  + F.sum v13
  + F.sum v14
  + F.sum v23
  + F.sum v24
  + F.sum v34
  where
    v12 = F.zipWith (*) v1 v2
    v13 = F.zipWith (*) v1 v3
    v14 = F.zipWith (*) v1 v4
    v23 = F.zipWith (*) v2 v3
    v24 = F.zipWith (*) v2 v4
    v34 = F.zipWith (*) v3 v4
