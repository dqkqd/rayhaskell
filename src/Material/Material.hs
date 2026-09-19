module Material.Material (
  Material (Lambertian, Metal, Dielectric),
  Scatter (Scatter, scatterAttenuation, scatterRay),
) where

import Color (Color)
import Ray (Ray)

data Material
  = Lambertian
      Color --  albedo
  | Metal
      Color --  albedo
      Double -- fuzziness
  | Dielectric
      Double -- reflection index
  deriving (Show)

data Scatter = Scatter
  { scatterAttenuation :: Color
  , scatterRay :: Ray
  }
