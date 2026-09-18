module Hit (
  Hittable (hit),
  HitRecord (HitRecord),
  Interval,
  defaultInterval,
  hitColor,
  inInterval,
) where

import Color (Color (C))
import Point (Point)
import Ray (Ray (Ray))
import Vec3 (Vec3 (V3), unit, (^*))

-- | A hit data structure, contains an origin and a normal vector pointing outward
data HitRecord
  = HitRecord
      Point -- origin
      -- TODO: implement of normal vector
      (Vec3 Double) -- normal vector
  deriving (Show, Eq)

data Interval
  = Interval
      Double -- min
      Double -- max

defaultInterval :: Interval
defaultInterval = Interval (-(1 / 0)) (1 / 0)

-- | Whether an interval contains value
--
-- >>> inInterval (Interval 1 10) 5
-- True
--
-- >>> inInterval (Interval 1 10) 12
-- False
--
-- >>> inInterval (Interval 1 10) 0
-- False
inInterval :: Interval -> Double -> Bool
inInterval (Interval minV maxV) value = value >= minV && value <= maxV

-- | Hittable interface, whether an object can be _hit_ by a ray.
class Hittable a where
  hit :: a -> Ray -> Interval -> Maybe HitRecord

hitColor :: (Hittable o) => o -> Ray -> Interval -> Color
hitColor object ray@(Ray _ direction) interval = case hit object ray interval of
  Just (HitRecord _ normalVec) -> C ((normalVec + 1) * 0.5)
  Nothing -> C (V3 1 1 1 ^* (1 - a) + V3 0.5 0.7 1.0 ^* a)
 where
  V3 _ y _ = unit direction
  a = 0.5 * (y + 1.0)
