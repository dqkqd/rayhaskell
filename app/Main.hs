module Main (main) where

import System.Environment (getArgs)
import System.IO (IOMode (WriteMode), withFile)

import Camera (
  CameraConfig (
    CameraConfig,
    imageWidthConfig,
    maxDepthConfig,
    ratioConfig,
    samplesPerPixelConfig
  ),
  createCamera,
  render,
 )
import Material.Material (Material (Lambertian))
import Point (point)
import Sphere (Sphere (Sphere, sphereCenter, sphereMaterial, sphereRadius))
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
          }
      camera = createCamera cameraConfig
      world =
        [ S
            ( Sphere
                { sphereCenter = point 0 0 (-1)
                , sphereRadius = 0.5
                , sphereMaterial = Lambertian 0
                }
            )
        , S
            ( Sphere
                { sphereCenter = point 0 (-100.5) (-1)
                , sphereRadius = 100
                , sphereMaterial = Lambertian 0
                }
            )
        ]

  withFile output WriteMode $ render camera world
