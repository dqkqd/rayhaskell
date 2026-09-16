module Ray (Ray (Ray), rayColor, rayAt, Hitable, hit, normalVec) where

import Color (Color (C))
import Point (Point, (.+^))
import Vec3 (Vec3 (V3), unit, (^*))

data Ray
  = Ray
      Point -- origin
      (Vec3 Double) -- direction

class Hitable a where
  hit :: a -> Ray -> Maybe Point
  normalVec :: a -> Point -> Vec3 Double

rayAt :: Ray -> Double -> Point
rayAt (Ray origin direction) t = origin .+^ (direction ^* t)

rayColor :: (Hitable o) => Ray -> o -> Color
rayColor ray@(Ray _ direction) object = case hit object ray of
  Just point -> C ((normalVec object point + 1) * 0.5)
  Nothing -> C (V3 1 1 1 ^* (1 - a) + V3 0.5 0.7 1.0 ^* a)
 where
  V3 _ y _ = unit direction
  a = 0.5 * (y + 1.0)
