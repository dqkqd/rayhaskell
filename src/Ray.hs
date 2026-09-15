module Ray (Ray (Ray), rayColor) where

import Color (Color)
import Vec3 (Vec3 (Vec3), unit, (*^))

data Ray = Ray
  { _rayOrigin :: Vec3 Double
  , rayDirection :: Vec3 Double
  }
  deriving (Eq)

rayColor :: Ray -> Color
rayColor r = Vec3 1.0 1.0 1.0 *^ (1 - a) + Vec3 0.5 0.7 1.0 *^ a
 where
  Vec3 _ y _ = unit $ rayDirection r
  a = 0.5 * (y + 1.0)
