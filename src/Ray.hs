module Ray (Ray (Ray), rayColor) where

import Color (Color (C))
import Control.Monad (guard)
import Point (Point (P), (.+^), (.-.))
import Vec3 (Vec3 (V3), dot, unit, (^*))

data Ray
  = Ray
      Point -- origin
      (Vec3 Double) -- direction

rayAt :: Ray -> Double -> Point
rayAt (Ray origin direction) t = origin .+^ (direction ^* t)

rayColor :: Ray -> Color
rayColor ray@(Ray _ direction) = case hit of
  Just point ->
    let normalVec = unit (point .-. center)
     in C ((normalVec + 1) * 0.5)
  Nothing -> C (V3 1 1 1 ^* (1 - a) + V3 0.5 0.7 1.0 ^* a)
 where
  center = P (V3 0 0 (-1))
  sphere = Sphere center 0.5
  hit = hitSphere sphere ray
  V3 _ y _ = unit direction
  a = 0.5 * (y + 1.0)

data Sphere
  = Sphere
      Point -- center
      Double -- radius

hitSphere :: Sphere -> Ray -> Maybe Point
hitSphere (Sphere center radius) ray@(Ray origin direction) = do
  let a = direction `dot` direction
  let b = direction `dot` (center .-. origin)
  let c = (center .-. origin) `dot` (center .-. origin) - radius * radius
  let delta = b * b - a * c
  guard (delta >= 0)
  let root = (b - sqrt delta) / a
  return (rayAt ray root)
