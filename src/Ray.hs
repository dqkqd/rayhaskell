module Ray (Ray (Ray), HitRecord (HitRecord), rayColor, rayAt, Hittable, hit) where

import Color (Color (C))
import Point (Point, (.+^))
import Vec3 (Vec3 (V3), unit, (^*))

data Ray
  = Ray
      Point -- origin
      (Vec3 Double) -- direction

data HitRecord
  = HitRecord
      Point -- origin
      (Vec3 Double) -- normal vector

class Hittable a where
  hit :: a -> Ray -> Maybe HitRecord

rayAt :: Ray -> Double -> Point
rayAt (Ray origin direction) t = origin .+^ (direction ^* t)

rayColor :: (Hittable o) => Ray -> o -> Color
rayColor ray@(Ray _ direction) object = case hit object ray of
  Just (HitRecord _ normalVec) -> C ((normalVec + 1) * 0.5)
  Nothing -> C (V3 1 1 1 ^* (1 - a) + V3 0.5 0.7 1.0 ^* a)
 where
  V3 _ y _ = unit direction
  a = 0.5 * (y + 1.0)
