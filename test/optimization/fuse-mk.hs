import qualified Data.Vector.Fixed         as F
import qualified Data.Vector.Fixed.Cont    as C
import           Data.Vector.Fixed.Unboxed (Vec)
import Data.Vector.Fixed (S,Z)

fini :: C.ContVec (S (S (S Z))) Double -> Double
fini = C.runContVec C.sum
{-# NOINLINE fini #-}

gen :: C.ContVec (S (S (S Z))) Double
gen = C.generate fromIntegral
{-# NOINLINE gen #-}

main :: IO ()
main = do 
  -- print $ F.sum $ (F.mk3 1 2 3 :: Vec3 Double)
  print $ fini (C.cvec (C.vector gen :: Vec (S (S (S Z))) Double))
