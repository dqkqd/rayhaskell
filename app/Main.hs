module Main (main) where

import System.Environment (getArgs)
import System.IO (IOMode (WriteMode), withFile)

import Camera (
  CameraConfig (
    CameraConfig,
    fieldOfViewConfig,
    imageWidthConfig,
    maxDepthConfig,
    ratioConfig,
    samplesPerPixelConfig
  ),
  createCamera,
  render,
 )
import Color (color)
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
          , fieldOfViewConfig = 90
          }
      camera = createCamera cameraConfig

      materialLeft = Lambertian (color 0 0 1)
      materialRight = Lambertian (color 1 0 0)

      r = cos (pi / 4)

      world =
        [ S
            ( Sphere
                { sphereCenter = point (-r) 0 (-1)
                , sphereRadius = r
                , sphereMaterial = materialLeft
                }
            )
        , S
            ( Sphere
                { sphereCenter = point r 0 (-1)
                , sphereRadius = r
                , sphereMaterial = materialRight
                }
            )
        ]

  withFile output WriteMode $ render camera world
