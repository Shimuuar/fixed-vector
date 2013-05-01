import qualified Data.Vector.Fixed         as F
import           Data.Vector.Fixed.Unboxed (Vec3)

main :: IO ()
main =
  print $ F.sum (F.generate fromIntegral :: Vec3 Double)
