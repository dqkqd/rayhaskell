module Hit.HitRecord (
  HitRecord (HitRecord),
  hitPoint,
  hitRay,
  hitDistance,
  hitNormalVec,
  hitMaterial,
  hitFrontFace,
) where

import Material.Material (Material)
import Point (Point)
import Ray (Ray, RayDistance)
import Vec3 (Vec3)

-- | A hit data structure, contains an origin and a normal vector pointing outward
data HitRecord
  = HitRecord
  { hitPoint :: Point
  , hitRay :: Ray
  , hitFrontFace :: Bool
  , hitMaterial :: Material
  , hitDistance :: RayDistance
  , hitNormalVec :: Vec3 Double -- normal vector
  }
  deriving (Show)
