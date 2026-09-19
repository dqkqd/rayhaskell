module Material.Impl (materialScatter) where

import Control.Monad.Random (Rand, StdGen)

import Color (color)
import Hit.HitRecord (
  HitRecord (hitFrontFace, hitMaterial, hitNormalVec, hitPoint, hitRay),
 )
import Material.Material (
  Material (Dielectric, Lambertian, Metal),
  Scatter (Scatter, scatterAttenuation, scatterRay),
 )
import Ray (Ray (Ray, rayDirection, rayOrigin))
import Vec3 (dot, nearZero, randomUnitVec, reflect, refract, unit, (^*))

materialScatter ::
  HitRecord -> -- the current hit record
  Rand StdGen (Maybe Scatter)
materialScatter hit = materialScatter' (hitMaterial hit) hit

-- | Material scatter implementation for different material
materialScatter' ::
  Material -> -- the material
  HitRecord -> -- the current hit record
  Rand StdGen (Maybe Scatter)
-- Lambertian
materialScatter' (Lambertian albedo) hit = do
  unitVec <- randomUnitVec
  let
    direction = unitVec + hitNormalVec hit
    scatterDirection =
      if nearZero direction
        then hitNormalVec hit
        else direction
  return $
    Just $
      Scatter
        { scatterAttenuation = albedo
        , scatterRay = Ray{rayOrigin = hitPoint hit, rayDirection = scatterDirection}
        }

-- Metal
materialScatter' (Metal albedo fuzz) hit = do
  unitVec <- randomUnitVec
  let
    scatterDirection =
      reflect (rayDirection (hitRay hit)) (hitNormalVec hit)
        + (unitVec ^* fuzz)
  return $
    if (scatterDirection `dot` hitNormalVec hit) > 0
      then
        Just $
          Scatter
            { scatterAttenuation = albedo
            , scatterRay = Ray{rayOrigin = hitPoint hit, rayDirection = scatterDirection}
            }
      else Nothing

-- Dielectric
materialScatter' (Dielectric refactionIndex) hit = do
  let
    etaRatio =
      if hitFrontFace hit
        then 1.0 / refactionIndex
        else refactionIndex

    unitDirection = unit $ rayDirection $ hitRay hit
    cosTheta = min 1.0 (unitDirection `dot` hitNormalVec hit)
    sinTheta = sqrt (1 - cosTheta * cosTheta)

    cannotRefract = etaRatio * sinTheta > 1.0

    scatterDirection =
      if cannotRefract
        then reflect unitDirection (hitNormalVec hit)
        else refract unitDirection (hitNormalVec hit) etaRatio

  return $
    Just $
      Scatter
        { scatterAttenuation = color 1 1 1
        , scatterRay = Ray{rayOrigin = hitPoint hit, rayDirection = scatterDirection}
        }
