module Ray (Ray (Ray), rayColor) where

import Color (Color)
import Vec3 (Vec3 (Vec3), dot, unit, (*^))

data Ray
  = Ray
      (Vec3 Double) -- origin
      (Vec3 Double) -- direction
  deriving (Eq)

rayColor :: Ray -> Color
rayColor ray@(Ray _ direction) =
  if hit
    then
      Vec3 1 0 0
    else
      Vec3 1 1 1 *^ (1 - a) + Vec3 0.5 0.7 1.0 *^ a
 where
  sphere = Sphere (Vec3 0 0 (-1)) 0.5
  hit = hitSphere sphere ray
  Vec3 _ y _ = unit direction
  a = 0.5 * (y + 1.0)

data Sphere
  = Sphere
      (Vec3 Double) -- center
      Double -- radius

hitSphere :: Sphere -> Ray -> Bool
hitSphere (Sphere center radius) (Ray origin direction) = delta >= 0
 where
  a = direction `dot` direction
  b = (-2) * (direction `dot` (center - origin))
  c = (center - origin) `dot` (center - origin) - radius * radius
  delta = b * b - 4 * a * c
