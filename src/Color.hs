module Color (Color (C), color, black) where

import Control.Parallel.Strategies (NFData)
import Interval (Interval (Interval), intervalClamp)
import Vec3 (Vec3 (V3), mkVec3)

-- | The internal color, in space [0 - 1]
--
-- Its internal is Vec3 Double
--
-- >>> (color 0.25 0.5 0.75 + color 0.25 0.25 0.25) == color 0.50 0.75 1.0
-- True
--
-- >>> (color 0.25 0.5 0.0) * 2 == color 0.50 1.0 1.0
-- True
newtype Color = C (Vec3 Double) deriving (Eq, Num, Fractional, NFData)

-- | Color constructor
color :: Double -> Double -> Double -> Color
color = mkVec3 C

-- | The black color
black :: Color
black = color 0 0 0

-- | The actual printted color in the file, its internal color space is
-- converted to [0 - 256)
-- >>> color 0.1 0.2 0.3
-- 25 51 76
instance Show Color where
  show (C c) = show x ++ " " ++ show y ++ " " ++ show z
   where
    interval = Interval 0.0 0.999
    (V3 x y z) :: (Vec3 Int) = floor . (* 256) . intervalClamp interval . linearToGamma <$> c

-- | Convert image data from linear to gamma space
linearToGamma :: Double -> Double
linearToGamma = sqrt . max 0
