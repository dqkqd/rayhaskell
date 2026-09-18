module World (WorldObject (S)) where

import Hit (Hittable (hitDistance, outwardNormalVec))
import Sphere (Sphere)

newtype WorldObject = S Sphere

instance Hittable WorldObject where
  hitDistance (S s) = hitDistance s
  outwardNormalVec (S s) = outwardNormalVec s
