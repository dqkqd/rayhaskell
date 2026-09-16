module Sphere (Sphere (Sphere)) where

import Control.Monad (guard)
import Point (Point, (.-.))
import Ray (Hitable, Ray (Ray), hit, normalVec, rayAt)
import Vec3 (dot, unit)

data Sphere
  = Sphere
      Point -- center
      Double -- radius

instance Hitable Sphere where
  hit (Sphere center radius) ray@(Ray origin direction) = do
    let a = direction `dot` direction
    let b = direction `dot` (center .-. origin)
    let c = (center .-. origin) `dot` (center .-. origin) - radius * radius
    let delta = b * b - a * c
    guard (delta >= 0)
    let root = (b - sqrt delta) / a
    return (rayAt ray root)

  normalVec (Sphere center _) point = unit (point .-. center)
