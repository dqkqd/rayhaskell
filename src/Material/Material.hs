module Material.Material (Material (Lambertian, Metal), Scatter (Scatter, scatterColor, scatterRay)) where

import Color (Color)
import Ray (Ray)

data Material
  = Lambertian
      Color --  albedo
  | Metal
      Color --  albedo
  deriving (Show)

data Scatter = Scatter
  { scatterColor :: Color
  , scatterRay :: Ray
  }
