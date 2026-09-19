module World (WorldObject (S)) where

import Hit.Hittable (Hittable (distance, outwardNormalVec))
import Sphere (Sphere)

newtype WorldObject = S Sphere

instance Hittable WorldObject where
  distance (S s) = distance s
  outwardNormalVec (S s) = outwardNormalVec s
