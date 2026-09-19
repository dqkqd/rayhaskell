module World (WorldObject (S)) where

import Hit.Hittable (Hittable (distance, material, outwardNormalVec))
import Sphere (Sphere)

newtype WorldObject = S Sphere

instance Hittable WorldObject where
  distance (S s) = distance s
  outwardNormalVec (S s) = outwardNormalVec s
  material (S s) = material s
