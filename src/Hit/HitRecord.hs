module Hit.HitRecord (HitRecord (HitRecord), hitPoint, hitRay, hitDistance, hitNormalVec) where

import Point (Point)
import Ray (Ray, RayDistance)
import Vec3 (Vec3)

-- | A hit data structure, contains an origin and a normal vector pointing outward
data HitRecord
  = HitRecord
  { hitPoint :: Point
  , hitRay :: Ray
  , hitDistance :: RayDistance
  , hitNormalVec :: Vec3 Double -- normal vector
  }
  deriving (Show)
