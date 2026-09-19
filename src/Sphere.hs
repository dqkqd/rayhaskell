module Sphere (Sphere (Sphere, sphereCenter, sphereRadius, sphereMaterial)) where

import Control.Monad (guard)
import Data.List (find)

import Hit.Hittable (Hittable (distance, material, outwardNormalVec))
import Interval (intervalSurrounds)
import Material.Material (Material)
import Point (Point, (.-.))
import Ray (Ray (Ray, rayDirection, rayOrigin), RayDistance (RayDistance))
import Vec3 (dot, unit)

data Sphere
  = Sphere
  { sphereCenter :: Point
  , sphereRadius :: Double
  , sphereMaterial :: Material
  }
  deriving (Show)

-- | A Sphere is a Hittable object
instance Hittable Sphere where
  distance
    Sphere{sphereCenter = sphereCenter, sphereRadius = sphereRadius}
    Ray{rayOrigin = rayOrigin, rayDirection = rayDirection}
    interval = do
      let a = rayDirection `dot` rayDirection
          b = rayDirection `dot` (sphereCenter .-. rayOrigin)
          c =
            (sphereCenter .-. rayOrigin) `dot` (sphereCenter .-. rayOrigin)
              - sphereRadius * sphereRadius
          delta = b * b - a * c

      guard (delta >= 0)
      let sqrtD = sqrt delta
      root <- find (intervalSurrounds interval) [(b - sqrtD) / a, (b + sqrtD) / a]
      return (RayDistance root)

  outwardNormalVec s p = unit (p .-. sphereCenter s)

  material = sphereMaterial
