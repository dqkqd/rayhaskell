module Hit (Hittable (hit), HitRecord (HitRecord), hitColor) where

import Color (Color (C))
import Point (Point)
import Ray (Ray (Ray))
import Vec3 (Vec3 (V3), unit, (^*))

data HitRecord
  = HitRecord
      Point -- origin
      (Vec3 Double) -- normal vector

class Hittable a where
  hit :: a -> Ray -> Maybe HitRecord

hitColor :: (Hittable o) => Ray -> o -> Color
hitColor ray@(Ray _ direction) object = case hit object ray of
  Just (HitRecord _ normalVec) -> C ((normalVec + 1) * 0.5)
  Nothing -> C (V3 1 1 1 ^* (1 - a) + V3 0.5 0.7 1.0 ^* a)
 where
  V3 _ y _ = unit direction
  a = 0.5 * (y + 1.0)
