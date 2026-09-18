module Hit (
  Hittable (outwardNormalVec, hitDistance),
  Interval,
  defaultInterval,
  hitMany,
  hitColor,
  inInterval,
) where

import Data.Maybe (mapMaybe)
import Safe (minimumMay)

import Color (Color (C))
import Point (Point)
import Ray (Ray (Ray), RayDistance, rayAt)
import Vec3 (Vec3 (V3), dot, unit, (^*))

-- | A hit data structure, contains an origin and a normal vector pointing outward
data HitRecord
  = HitRecord
      Point -- origin
      RayDistance -- distance with the ray
      -- TODO: implement of normal vector
      (Vec3 Double) -- normal vector
  deriving (Show, Eq)

instance Ord HitRecord where
  compare (HitRecord _ dist1 _) (HitRecord _ dist2 _) = compare dist1 dist2

data Interval
  = Interval
      Double -- min
      Double -- max

defaultInterval :: Interval
defaultInterval = Interval 0 (1 / 0)

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
  -- | Calculate the hit distance from a ray
  hitDistance :: a -> Ray -> Interval -> Maybe RayDistance

  -- | Calculate the outward normal vector at a given point
  outwardNormalVec :: a -> Point -> Vec3 Double

  -- | Calculate the hit record
  hit :: a -> Ray -> Interval -> Maybe HitRecord
  hit object ray@(Ray _ direction) interval = do
    rayDistance <- hitDistance object ray interval
    let hitPoint = rayAt ray rayDistance
        outwardNormal = outwardNormalVec object hitPoint
        normalVec =
          if (outwardNormal `dot` direction) > 0.0
            then -outwardNormal -- ray is inside
            else outwardNormal -- ray is outside
    return (HitRecord hitPoint rayDistance normalVec)

-- | Given many objects, we want to find the closest hit record
hitMany :: (Hittable o) => [o] -> Ray -> Interval -> Maybe HitRecord
hitMany objects ray interval = minimumMay hitRecords
 where
  hitRecords = mapMaybe (\object -> hit object ray interval) objects

-- | Background default color
defaultColor :: Ray -> Color
defaultColor (Ray _ direction) = C (V3 1 1 1 ^* (1 - a) + V3 0.5 0.7 1.0 ^* a)
 where
  V3 _ y _ = unit direction
  a = 0.5 * (y + 1.0)

-- | Color from a hit record
hitColor :: Ray -> Maybe HitRecord -> Color
hitColor ray Nothing = defaultColor ray
hitColor _ (Just (HitRecord _ _ normalVec)) = C ((normalVec + 1) * 0.5)
