import qualified Data.Vector.Fixed         as F
import           Data.Vector.Fixed.Unboxed (Vec3)

main :: IO ()
main = do
  let v = F.generate ((*2).fromIntegral) :: Vec3 Double
      u = F.replicate 12
  print $ F.sum $ F.zipWith (*) v u
