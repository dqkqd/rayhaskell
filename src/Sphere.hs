module Sphere (Sphere (Sphere)) where

import Control.Monad (guard)
import Data.List (find)
import Hit (Hittable (hit, outwardNormalVec), inInterval, mkHitRecord)
import Point (Point, (.-.))
import Ray (Ray (Ray), rayAt)
import Vec3 (dot, unit)

data Sphere
  = Sphere
      Point -- center
      Double -- radius
  deriving (Show)

-- | A Sphere is a Hittable object
instance Hittable Sphere where
  hit sphere@(Sphere sCenter sRadius) ray@(Ray rOrigin rDirection) interval = do
    let a = rDirection `dot` rDirection
        b = rDirection `dot` (sCenter .-. rOrigin)
        c = (sCenter .-. rOrigin) `dot` (sCenter .-. rOrigin) - sRadius * sRadius
        delta = b * b - a * c

    guard (delta >= 0)
    let sqrtD = sqrt delta
    root <- find (inInterval interval) [(b - sqrtD) / a, (b + sqrtD) / a]

    let hitPoint = rayAt ray root
    return (mkHitRecord sphere ray hitPoint)

  outwardNormalVec (Sphere center _) p = unit (p .-. center)
