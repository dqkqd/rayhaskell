module Sphere (Sphere (Sphere)) where

import Control.Monad (guard)
import Hit (HitRecord (HitRecord), Hittable (hit))
import Point (Point, (.-.))
import Ray (Ray (Ray), rayAt)
import Vec3 (dot, unit)

data Sphere
  = Sphere
      Point -- center
      Double -- radius

instance Hittable Sphere where
  hit (Sphere center radius) ray@(Ray origin direction) = do
    let a = direction `dot` direction
    let b = direction `dot` (center .-. origin)
    let c = (center .-. origin) `dot` (center .-. origin) - radius * radius
    let delta = b * b - a * c
    guard (delta >= 0)

    let t = (b - sqrt delta) / a
    let hitPoint = rayAt ray t
    let normalVec = unit (hitPoint .-. center)
    return (HitRecord hitPoint normalVec)
