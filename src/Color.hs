module Color (Color, formatColor) where

import Vec3 (Vec3 (Vec3), (^*))

type Color = Vec3 Double

formatColor :: Color -> String
formatColor c = show x ++ " " ++ show y ++ " " ++ show z
 where
  (Vec3 x y z) :: (Vec3 Int) = floor <$> (255.999 ^* c)
