module Point (Point (P), (.-.), (.+^), (.-^)) where

import Vec3 (Vec3)

newtype Point = P (Vec3 Double)

(.-.) :: Point -> Point -> Vec3 Double
(.-.) (P u) (P v) = u - v

(.+^) :: Point -> Vec3 Double -> Point
(.+^) (P u) v = P (u + v)

(.-^) :: Point -> Vec3 Double -> Point
(.-^) (P u) v = P (u - v)
