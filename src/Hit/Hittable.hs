module Hit.Hittable (
  Hittable (outwardNormalVec, distance, material),
  hitMany,
) where

import Data.Maybe (mapMaybe)
import Safe (minimumByMay)

import Data.Ord (comparing)
import Hit.HitRecord (
  HitRecord (HitRecord, hitDistance, hitMaterial, hitNormalVec, hitPoint, hitRay),
 )
import Interval (Interval)
import Material.Material (Material)
import Point (Point)
import Ray (Ray (rayDirection), RayDistance, rayAt)
import Vec3 (Vec3, dot)

-- | Hittable interface, whether an object can be _hit_ by a ray.
class Hittable a where
  -- | Calculate the hit distance from a ray
  distance :: a -> Ray -> Interval -> Maybe RayDistance

  -- | Calculate the outward normal vector at a given point
  outwardNormalVec :: a -> Point -> Vec3 Double

  material :: a -> Material

-- | Calculate the hit record
hitSingle :: (Hittable a) => a -> Ray -> Interval -> Maybe HitRecord
hitSingle object ray interval = do
  rayDistance <- distance object ray interval
  let hitPoint = rayAt ray rayDistance
      outwardNormal = outwardNormalVec object hitPoint
      normalVec =
        if (outwardNormal `dot` rayDirection ray) > 0.0
          then -outwardNormal -- ray is inside
          else outwardNormal -- ray is outside
  return
    HitRecord
      { hitPoint = hitPoint
      , hitRay = ray
      , hitMaterial = material object
      , hitDistance = rayDistance
      , hitNormalVec = normalVec
      }

-- | Given many objects, we want to find the closest hit record
hitMany :: (Hittable o) => [o] -> Ray -> Interval -> Maybe HitRecord
hitMany objects ray interval =
  minimumByMay (comparing hitDistance) hitRecords
 where
  hitRecords = mapMaybe (\object -> hitSingle object ray interval) objects
