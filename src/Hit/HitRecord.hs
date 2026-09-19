module Hit.HitRecord (HitRecord (HitRecord), hitPoint, hitDistance, hitNormalVec) where

import Point (Point)
import Ray (RayDistance)
import Vec3 (Vec3)

-- | A hit data structure, contains an origin and a normal vector pointing outward
data HitRecord
  = HitRecord
  { hitPoint :: Point
  , hitDistance :: RayDistance
  , hitNormalVec :: Vec3 Double -- normal vector
  }
  deriving (Show)
