module Color (Color, formatColor) where

import Vec3 (Vec3, toTuple)

type Color = Vec3

formatColor :: Color -> String
formatColor c = x ++ " " ++ y ++ " " ++ z
 where
  scaled = show . (floor :: Double -> Int) <$> c * 255.999
  (x, y, z) = toTuple scaled
