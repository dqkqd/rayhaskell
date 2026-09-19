module Hit.Hittable (
  Hittable (outwardNormalVec, distance, material),
  hitMany,
) where

import Safe (minimumByMay)

import Data.Ord (comparing)
import Hit.HitRecord (
  HitRecord (
    HitRecord,
    hitDistance,
    hitFrontFace,
    hitMaterial,
    hitNormalVec,
    hitPoint,
    hitRay
  ),
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

-- | Compute the hit record at a distance
hitRecordAt :: (Hittable a) => a -> Ray -> RayDistance -> HitRecord
hitRecordAt object ray rayDistance =
  let
    hitPoint = rayAt ray rayDistance
    outwardNormal = outwardNormalVec object hitPoint
    frontFace = (outwardNormal `dot` rayDirection ray) < 0.0
    normalVec =
      if frontFace
        then outwardNormal -- ray it outside
        else -outwardNormal -- ray is inside
   in
    HitRecord
      { hitPoint = hitPoint
      , hitRay = ray
      , hitFrontFace = frontFace
      , hitMaterial = material object
      , hitDistance = rayDistance
      , hitNormalVec = normalVec
      }

-- | Given many objects, we want to find the closest hit record
hitMany :: (Hittable o) => [o] -> Ray -> Interval -> Maybe HitRecord
hitMany objects ray interval = do
  let distances = [(o, d) | o <- objects, Just d <- [distance o ray interval]]
  (object, minDistance) <- minimumByMay (comparing snd) distances
  return $ hitRecordAt object ray minDistance
