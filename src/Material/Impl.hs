module Material.Impl (materialScatter) where

import Control.Monad.Random (Rand, StdGen)
import Hit.HitRecord (HitRecord (hitMaterial, hitNormalVec, hitPoint))
import Material.Material (
  Material (Lambertian, Metal),
  Scatter (Scatter, scatterColor, scatterRay),
 )
import Ray (Ray (Ray, rayDirection, rayOrigin))
import Vec3 (randomUnitVec)

materialScatter ::
  HitRecord -> -- the current hit record
  Rand StdGen Scatter
materialScatter hit = materialScatter' (hitMaterial hit) hit

materialScatter' ::
  Material -> -- the material
  HitRecord -> -- the current hit record
  Rand StdGen Scatter
materialScatter' (Lambertian albedo) hit = do
  unitVec <- randomUnitVec
  let
    scatterDirection = unitVec + hitNormalVec hit
  return
    Scatter
      { scatterColor = albedo
      , scatterRay = Ray{rayOrigin = hitPoint hit, rayDirection = scatterDirection}
      }
materialScatter' (Metal _) _ = undefined
