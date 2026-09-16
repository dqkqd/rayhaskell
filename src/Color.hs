module Color (Color (C)) where

import Vec3 (Vec3 (V3))

newtype Color = C (Vec3 Double)

instance Show Color where
  show (C c) = show x ++ " " ++ show y ++ " " ++ show z
   where
    (V3 x y z) :: (Vec3 Int) = floor <$> (255.999 * c)
