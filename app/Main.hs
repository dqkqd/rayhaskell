module Main (main) where

import System.Environment (getArgs)
import System.IO (IOMode (WriteMode), withFile)

import Camera (
  CameraConfig (
    CameraConfig,
    defocusAngleConfig,
    fieldOfViewConfig,
    focusDistConfig,
    imageWidthConfig,
    lookAtConfig,
    lookFromConfig,
    maxDepthConfig,
    ratioConfig,
    samplesPerPixelConfig,
    viewUpConfig
  ),
  createCamera,
  render,
 )
import Color (Color (C), color)
import Control.Monad.Random (
  MonadRandom (getRandom, getRandomR),
  Rand,
  StdGen,
  evalRandIO,
  forM,
 )
import Data.Maybe (catMaybes)
import Interval (Interval (Interval))
import Material.Material (Material (Dielectric, Lambertian, Metal))
import Point (point, (.-.))
import Sphere (Sphere (Sphere, sphereCenter, sphereMaterial, sphereRadius))
import Vec3 (Vec3 (V3), lengthSquare, randomVec, randomVecR)
import World (WorldObject (S))

main :: IO ()
main = do
  args <- getArgs
  let output = case args of
        [filename] -> filename
        _ -> "output.ppm"

      cameraConfig =
        CameraConfig
          { ratioConfig = 16.0 / 9.0
          , imageWidthConfig = 400
          , samplesPerPixelConfig = 100
          , maxDepthConfig = 50
          , fieldOfViewConfig = 20
          , lookFromConfig = point 13 2 3
          , lookAtConfig = point 0 0 0
          , viewUpConfig = V3 0 1 0
          , defocusAngleConfig = 0.6
          , focusDistConfig = 10.0
          }
      camera = createCamera cameraConfig

      defaultObject =
        [ S
            ( Sphere
                { sphereCenter = point 0 (-1000) (-1)
                , sphereRadius = 1000
                , sphereMaterial = Lambertian (color 0.5 0.5 0.5)
                }
            )
        , S
            ( Sphere
                { sphereCenter = point 0 1 0
                , sphereRadius = 1.0
                , sphereMaterial = Dielectric 1.5
                }
            )
        , S
            ( Sphere
                { sphereCenter = point (-4) 1 0
                , sphereRadius = 1.0
                , sphereMaterial = Lambertian (color 0.4 0.2 0.1)
                }
            )
        , S
            ( Sphere
                { sphereCenter = point 4 1 0
                , sphereRadius = 1.0
                , sphereMaterial = Metal (color 0.7 0.6 0.5) 0
                }
            )
        ]

  randomObjects <-
    evalRandIO
      ( forM [(a, b) | a <- [(-11 :: Int) .. 10], b <- [(-11 :: Int) .. 10]] $
          uncurry randomObject
      )

  let world = defaultObject ++ catMaybes randomObjects
  withFile output WriteMode $ render camera world

randomObject :: Int -> Int -> Rand StdGen (Maybe WorldObject)
randomObject a b = do
  dx <- getRandom
  dz <- getRandom
  material <- randomMaterial

  let
    center = point (fromIntegral a + dx * 0.9) 0.2 (fromIntegral b + 0.9 * dz)
    sphere =
      S
        ( Sphere
            { sphereCenter = center
            , sphereRadius = 0.2
            , sphereMaterial = material
            }
        )

  if lengthSquare (center .-. point 4 0.2 0) > 0.9 * 0.9
    then return $ Just sphere
    else return Nothing

randomMaterial :: Rand StdGen Material
randomMaterial = do
  chooseMat :: Double <- getRandom
  case () of
    _
      | chooseMat < 0.8 -> do
          -- diffuse
          c1 <- randomVec
          c2 <- randomVec
          let albedo = C (c1 * c2)
          return $ Lambertian albedo
      | chooseMat < 0.95 -> do
          -- metal
          albedo <- randomVecR (Interval 0.5 1)
          fuzz <- getRandomR (0, 0.5)
          return $ Metal (C albedo) fuzz
      | otherwise ->
          -- glass
          return $ Dielectric 1.5
