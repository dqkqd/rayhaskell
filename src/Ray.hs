module Ray (Ray (Ray), rayAt) where

import Point (Point, (.+^))
import Vec3 (Vec3, (^*))

-- $setup
-- >>> import Point (point)
-- >>> import Vec3 (Vec3 (V3))

-- | The Ray, it contains an origin and a vector direction
data Ray
  = Ray
      Point -- origin
      (Vec3 Double) -- direction
  deriving (Show)

-- | Where this ray is pointing to with a scaled direction
--
-- >>> ray = Ray (point 1 2 3) (V3 4 5 6)
-- >>> rayAt ray 2
-- P (V3 9.0 12.0 15.0)
rayAt ::
  Ray ->
  Double -> -- scaled
  Point
rayAt (Ray origin direction) scale = origin .+^ (direction ^* scale)
