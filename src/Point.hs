module Point (Point, (.-.), (.+^), (.-^), point) where

import Vec3 (Vec3, mkVec3)

-- $setup
-- >>> import Vec3 (Vec3 (V3))

newtype Point = P (Vec3 Double) deriving (Show, Eq)

-- | Point constructor
point :: Double -> Double -> Double -> Point
point = mkVec3 P

-- | Point - Point
-- `pointA - pointB = vectorAB`
-- >>> point 4 6 8 .-. point 1 2 3
-- V3 3.0 4.0 5.0
(.-.) :: Point -> Point -> Vec3 Double
(.-.) (P u) (P v) = u - v

-- | Point + Vector
-- `pointA + vectorAB` = pointB
--
-- >>> point 1 2 3 .+^ V3 3 4 5
-- P (V3 4.0 6.0 8.0)
(.+^) :: Point -> Vec3 Double -> Point
(.+^) (P u) v = P (u + v)

-- | Point - Vector
-- `pointA - vectorBA = pointA + vectorAB = pointB`
--
-- >>> point 1 2 3 .-^ V3 (-3) (-4) (-5)
-- P (V3 4.0 6.0 8.0)
(.-^) :: Point -> Vec3 Double -> Point
(.-^) (P u) v = P (u - v)
