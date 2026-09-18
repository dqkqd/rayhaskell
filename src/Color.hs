module Color (Color (C), color) where

import Vec3 (Vec3 (V3), mkVec3)

-- | The internal color, in space [0 - 1]
newtype Color = C (Vec3 Double) deriving (Eq)

-- | Color constructor
color :: Double -> Double -> Double -> Color
color = mkVec3 C

-- | The actual printted color in the file, its internal color space is
-- converted to [0 - 256)
-- >>> color 0.1 0.2 0.3
-- 25 51 76
instance Show Color where
  show (C c) = show x ++ " " ++ show y ++ " " ++ show z
   where
    (V3 x y z) :: (Vec3 Int) = floor <$> (255.999 * c)
