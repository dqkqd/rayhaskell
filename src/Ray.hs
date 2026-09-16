module Ray (Ray (Ray), rayColor) where

import Color (Color (C))
import Point (Point (P), (.-.))
import Vec3 (Vec3 (V3), dot, unit, (^*))

data Ray
  = Ray
      Point -- origin
      (Vec3 Double) -- direction

rayColor :: Ray -> Color
rayColor ray@(Ray _ direction) =
  if hit
    then
      C (V3 1 0 0)
    else
      C (V3 1 1 1 ^* (1 - a) + V3 0.5 0.7 1.0 ^* a)
 where
  origin = P (V3 0 0 (-1))
  sphere = Sphere origin 0.5
  hit = hitSphere sphere ray
  V3 _ y _ = unit direction
  a = 0.5 * (y + 1.0)

data Sphere
  = Sphere
      Point -- center
      Double -- radius

hitSphere :: Sphere -> Ray -> Bool
hitSphere (Sphere center radius) (Ray origin direction) = delta >= 0
 where
  a = direction `dot` direction
  b = (-2) * (direction `dot` (center .-. origin))
  c = (center .-. origin) `dot` (center .-. origin) - radius * radius
  delta = b * b - 4 * a * c
