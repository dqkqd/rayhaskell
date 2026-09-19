module Material.Impl (materialScatter) where

import Control.Monad.Random (Rand, StdGen)
import Hit.HitRecord (HitRecord (hitMaterial, hitNormalVec, hitPoint, hitRay))
import Material.Material (
  Material (Lambertian, Metal),
  Scatter (Scatter, scatterAttenuation, scatterRay),
 )
import Ray (Ray (Ray, rayDirection, rayOrigin))
import Vec3 (nearZero, randomUnitVec, reflect)

materialScatter ::
  HitRecord -> -- the current hit record
  Rand StdGen Scatter
materialScatter hit = materialScatter' (hitMaterial hit) hit

-- | Material scatter implementation for different material
materialScatter' ::
  Material -> -- the material
  HitRecord -> -- the current hit record
  Rand StdGen Scatter
-- Lambertian
materialScatter' (Lambertian albedo) hit = do
  unitVec <- randomUnitVec
  let
    direction = unitVec + hitNormalVec hit
    scatterDirection =
      if nearZero direction
        then hitNormalVec hit
        else direction
  return
    Scatter
      { scatterAttenuation = albedo
      , scatterRay = Ray{rayOrigin = hitPoint hit, rayDirection = scatterDirection}
      }

-- Metal
materialScatter' (Metal albedo) hit = do
  let scatterDirection = reflect (rayDirection (hitRay hit)) (hitNormalVec hit)
  return
    Scatter
      { scatterAttenuation = albedo
      , scatterRay = Ray{rayOrigin = hitPoint hit, rayDirection = scatterDirection}
      }
