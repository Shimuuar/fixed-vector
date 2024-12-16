{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=Test.Tasty.Inspection.Plugin #-}
{-# OPTIONS_GHC -dsuppress-idinfo #-}
module Main where

import Test.Tasty
import Test.Tasty.Inspection

import Data.Vector.Fixed           qualified as F
import Data.Vector.Fixed.Unboxed   qualified as FU
import Data.Vector.Fixed.Boxed     qualified as FB
import Data.Vector.Fixed.Primitive qualified as FP

import Inspect.Obligations



simple_fusion_FU :: Int -> Int
simple_fusion_FU n = F.sum $ F.generate @FU.Vec3 (*n)

simple_fusion_FB :: Int -> Int
simple_fusion_FB n = F.sum $ F.generate @FB.Vec3 (*n)

simple_fusion_FP :: Int -> Int
simple_fusion_FP n = F.sum $ F.generate @FP.Vec3 (*n)

fuse_mapM_ :: IO ()
fuse_mapM_ = F.mapM_ print (F.mk3 1 2 3 :: FU.Vec3 Double)

fuse_zipWith :: Int -> Int
fuse_zipWith n = F.sum $ F.zipWith (*) v u
  where v,u :: FU.Vec3 Int
        v = F.generate  (*2)
        u = F.replicate n

fuse_zipWith_self :: Int -> Int
fuse_zipWith_self n = F.sum $ F.zipWith (*) u u
  where u :: FU.Vec3 Int
        u = F.replicate n

-- More involved example with zipWith. It stresses optimizer and could be
-- used as a benchmark for optimization of compilation speed.
fuse_zipWithParam :: FP.Vec 3 Int -> FP.Vec 3 Int -> FP.Vec 3 Int -> Int
fuse_zipWithParam v1 v2 v3 = F.sum v12 + F.sum v13 + F.sum v23 where
  v12 = F.zipWith (*) v1 v2
  v13 = F.zipWith (*) v1 v3
  v23 = F.zipWith (*) v2 v3



main :: IO ()
main = defaultMain $ testGroup "inspect"
  [ $(inspectObligations [ hasNoTypeClasses
                         , noArrayAlloc
                         ] 'simple_fusion_FU)
  , $(inspectObligations [ hasNoTypeClasses
                         , noArrayAlloc
                         ] 'simple_fusion_FB)
  , $(inspectObligations [ hasNoTypeClasses
                         , noArrayAlloc
                         ] 'simple_fusion_FP)
  , $(inspectObligations [ hasNoTypeClasses
                         , noArrayAlloc
                         ] 'fuse_mapM_)
  , $(inspectObligations [ hasNoTypeClasses
                         , noArrayAlloc
                         ] 'fuse_zipWith)
  , $(inspectObligations [ hasNoTypeClasses
                         -- FIXME: Does not fuse when used nonlinearly
                         -- , noArrayAlloc
                         ] 'fuse_zipWith_self)
  , $(inspectObligations [ hasNoTypeClasses
                         , noArrayAlloc
                         ] 'fuse_zipWithParam)
  ]
