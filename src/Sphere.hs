module Sphere (Sphere (Sphere)) where

import Control.Monad (guard)
import Data.List (find)

import Hit (Hittable (hitDistance, outwardNormalVec))
import Interval (inInterval)
import Point (Point, (.-.))
import Ray (Ray (Ray), RayDistance (RayDistance))
import Vec3 (dot, unit)

data Sphere
  = Sphere
      Point -- center
      Double -- radius
  deriving (Show)

-- | A Sphere is a Hittable object
instance Hittable Sphere where
  hitDistance (Sphere sCenter sRadius) (Ray rOrigin rDirection) interval = do
    let a = rDirection `dot` rDirection
        b = rDirection `dot` (sCenter .-. rOrigin)
        c = (sCenter .-. rOrigin) `dot` (sCenter .-. rOrigin) - sRadius * sRadius
        delta = b * b - a * c

    guard (delta >= 0)
    let sqrtD = sqrt delta
    root <- find (inInterval interval) [(b - sqrtD) / a, (b + sqrtD) / a]
    return (RayDistance root)

  outwardNormalVec (Sphere center _) p = unit (p .-. center)
