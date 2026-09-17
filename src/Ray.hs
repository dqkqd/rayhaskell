module Ray (Ray (Ray), rayAt) where

import Point (Point, (.+^))
import Vec3 (Vec3, (^*))

data Ray
  = Ray
      Point -- origin
      (Vec3 Double) -- direction

rayAt :: Ray -> Double -> Point
rayAt (Ray origin direction) t = origin .+^ (direction ^* t)
