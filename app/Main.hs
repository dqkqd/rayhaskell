module Main (main) where

import System.Environment (getArgs)
import System.IO (IOMode (WriteMode), withFile)

import Camera (
  CameraConfig (CameraConfig, imageWidthConfig, ratioConfig),
  createCamera,
  render,
 )
import Point (point)
import Sphere (Sphere (Sphere))
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
          , imageWidthConfig = 800
          }
      camera = createCamera cameraConfig
      world =
        [ S (Sphere (point 0 0 (-1)) 0.5)
        , S (Sphere (point 0 (-100.5) (-1)) 100)
        ]

  withFile output WriteMode $ render camera world
