module Ray (Ray (Ray, rayOrigin, rayDirection), rayAt, RayDistance (RayDistance)) where

import Point (Point, (.+^))
import Vec3 (Vec3, (^*))

-- $setup
-- >>> import Point (point)
-- >>> import Vec3 (Vec3 (V3))

-- | The Ray, it contains an origin and a vector direction
data Ray
  = Ray
  { rayOrigin :: Point -- origin
  , rayDirection :: Vec3 Double -- direction
  }
  deriving (Show)

newtype RayDistance = RayDistance Double
  deriving (Eq, Ord, Num, Fractional, Floating, Show)

-- | Where this ray is pointing to with a scaled direction
--
-- >>> ray = Ray (point 1 2 3) (V3 4 5 6)
-- >>> rayAt ray (RayDistance 2)
-- P (V3 9.0 12.0 15.0)
rayAt ::
  Ray ->
  RayDistance ->
  Point
rayAt (Ray origin direction) (RayDistance d) = origin .+^ (direction ^* d)
