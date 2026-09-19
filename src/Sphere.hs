module Sphere (Sphere (Sphere, sphereCenter, sphereRadius, sphereMaterial)) where

import Control.Monad (guard)

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
      let
        oc = sphereCenter .-. rayOrigin
        a = rayDirection `dot` rayDirection
        b = rayDirection `dot` oc
        c = oc `dot` oc - sphereRadius * sphereRadius
        delta = b * b - a * c

      guard (delta >= 0)
      let
        sqrtD = sqrt delta
        r1 = (b - sqrtD) / a
        r2 = (b + sqrtD) / a

      if intervalSurrounds interval r1
        then return $ RayDistance r1
        else
          if intervalSurrounds interval r2
            then return $ RayDistance r2
            else
              Nothing

  outwardNormalVec s p = unit (p .-. sphereCenter s)

  material = sphereMaterial
