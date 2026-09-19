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
import Color (color)
import Material.Material (Material (Dielectric, Lambertian, Metal))
import Point (point)
import Sphere (Sphere (Sphere, sphereCenter, sphereMaterial, sphereRadius))
import Vec3 (Vec3 (V3))
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
          , lookFromConfig = point (-2) 2 1
          , lookAtConfig = point 0 0 (-1)
          , viewUpConfig = V3 0 1 0
          , defocusAngleConfig = 10
          , focusDistConfig = 3.4
          }
      camera = createCamera cameraConfig

      materialGround = Lambertian (color 0.8 0.8 0)
      materialCenter = Lambertian (color 0.1 0.2 0.5)
      materialLeft = Dielectric 1.5
      materialBubble = Dielectric (1 / 1.5)
      materialRight = Metal (color 0.8 0.6 0.2) 1.0

      world =
        [ S
            ( Sphere
                { sphereCenter = point 0 (-100.5) (-1)
                , sphereRadius = 100
                , sphereMaterial = materialGround
                }
            )
        , S
            ( Sphere
                { sphereCenter = point 0 0 (-1.2)
                , sphereRadius = 0.5
                , sphereMaterial = materialCenter
                }
            )
        , S
            ( Sphere
                { sphereCenter = point (-1.0) 0 (-1.0)
                , sphereRadius = 0.5
                , sphereMaterial = materialLeft
                }
            )
        , S
            ( Sphere
                { sphereCenter = point (-1.0) 0 (-1.0)
                , sphereRadius = 0.4
                , sphereMaterial = materialBubble
                }
            )
        , S
            ( Sphere
                { sphereCenter = point 1.0 0 (-1.0)
                , sphereRadius = 0.5
                , sphereMaterial = materialRight
                }
            )
        ]

  withFile output WriteMode $ render camera world
