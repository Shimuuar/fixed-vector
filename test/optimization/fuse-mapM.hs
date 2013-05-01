import qualified Data.Vector.Fixed         as F
import           Data.Vector.Fixed.Unboxed (Vec3)

main :: IO ()
main = do 
  F.mapM_ print (F.mk3 1 2 3 :: Vec3 Double)
