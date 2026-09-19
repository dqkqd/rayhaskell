module Material.Impl (materialScatter) where

import Control.Monad.Random (MonadRandom (getRandom), Rand, StdGen)

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
  double :: Double <- getRandom
  let
    ri =
      if hitFrontFace hit
        then 1.0 / refactionIndex
        else refactionIndex

    unitDirection = unit $ rayDirection $ hitRay hit
    cosTheta = min 1.0 ((-unitDirection) `dot` hitNormalVec hit)
    sinTheta = sqrt (1 - cosTheta * cosTheta)

    cannotRefract = ri * sinTheta > 1.0
    scatterDirection =
      if cannotRefract || (reflectance cosTheta ri > double)
        then reflect unitDirection (hitNormalVec hit)
        else refract unitDirection (hitNormalVec hit) ri

  return $
    Just $
      Scatter
        { scatterAttenuation = color 1 1 1
        , scatterRay = Ray{rayOrigin = hitPoint hit, rayDirection = scatterDirection}
        }
 where
  -- Schlick's approximation
  reflectance ::
    Double -> -- cosine
    Double -> -- refractionIndex
    Double
  reflectance
    cosine
    refractionIndex = r + (1 - r) * ((1 - cosine) ^ (5 :: Int))
     where
      r0 = (1 - refractionIndex) / (1 + refractionIndex)
      r = r0 * r0
