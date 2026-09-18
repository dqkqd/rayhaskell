module Sphere (Sphere (Sphere)) where

import Control.Monad (guard)
import Data.List (find)
import Hit (HitRecord (HitRecord), Hittable (hit), inInterval)
import Point (Point, (.-.))
import Ray (Ray (Ray), rayAt)
import Vec3 (Vec3, dot, unit)

-- $setup
-- >>> import Point (point)

data Sphere
  = Sphere
      Point -- center
      Double -- radius
  deriving (Show)

-- | A Sphere is a Hittable object
instance Hittable Sphere where
  hit sphere@(Sphere center radius) ray@(Ray origin direction) interval = do
    let a = direction `dot` direction
        b = direction `dot` (center .-. origin)
        c = (center .-. origin) `dot` (center .-. origin) - radius * radius
        delta = b * b - a * c

    guard (delta >= 0)
    let sqrtD = sqrt delta
    root <- find (inInterval interval) [(b - sqrtD) / a, (b + sqrtD) / a]

    let hitPoint = rayAt ray root
    return (HitRecord hitPoint (normalVec sphere hitPoint))

-- | Calculate normal vector
--
-- >>> let sphere = Sphere (point 2 2 0) 2
-- >>> let p = point 4 2 0
-- >>> normalVec sphere p
-- V3 1.0 0.0 0.0
normalVec :: Sphere -> Point -> Vec3 Double
normalVec (Sphere center _) p = unit (p .-. center)
