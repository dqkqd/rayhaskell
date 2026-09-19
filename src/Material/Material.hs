module Material.Material (
  Material (Lambertian, Metal),
  Scatter (Scatter, scatterAttenuation, scatterRay),
) where

import Color (Color)
import Ray (Ray)

data Material
  = Lambertian
      Color --  albedo
  | Metal
      Color --  albedo
  deriving (Show)

data Scatter = Scatter
  { scatterAttenuation :: Color
  , scatterRay :: Ray
  }
