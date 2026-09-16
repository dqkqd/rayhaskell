module Main (main) where

import System.Environment (getArgs)
import System.IO (IOMode (WriteMode), withFile)

import Color (Color)
import Image (Image (Image), writeImage)
import Point (Point (P), (.+^), (.-.), (.-^))
import Ray (Ray (Ray), rayColor)
import Vec3 (Vec3 (V3))

main :: IO ()
main = do
  args <- getArgs
  let output = case args of
        [filename] -> filename
        _ -> "output.ppm"
  withFile output WriteMode $ writeImage createImage

createImage :: Image
createImage = Image imageWidth imageHeight colors
 where
  ratio :: Double = 16.0 / 9.0
  imageWidth :: Int = 400
  imageHeight :: Int = floor (fromIntegral imageWidth / ratio)

  focalLength :: Double = 1.0
  viewportHeight :: Double = 2.0
  viewportWidth = viewportHeight * (fromIntegral imageWidth / fromIntegral imageHeight)
  cameraCenter = P (V3 0 0 0)

  viewportU = V3 viewportWidth 0 0
  viewportV = V3 0 (-viewportHeight) 0

  pixelDeltaU = viewportU / fromIntegral imageWidth
  pixelDeltaV = viewportV / fromIntegral imageHeight

  viewportUpperLeft =
    cameraCenter
      .-^ V3 0 0 focalLength
      .-^ (viewportU / 2)
      .-^ (viewportV / 2)

  pixel00Location = viewportUpperLeft .+^ (0.5 * pixelDeltaU) .+^ (0.5 * pixelDeltaV)

  colors =
    [ [ color i j
      | i <- [(0 :: Int) .. imageWidth - 1]
      ]
    | j <- [(0 :: Int) .. imageHeight - 1]
    ]

  color :: Int -> Int -> Color
  color i j = rayColor ray
   where
    pixelCenter =
      pixel00Location
        .+^ (pixelDeltaU * fromIntegral i)
        .+^ (pixelDeltaV * fromIntegral j)
    rayDirection = pixelCenter .-. cameraCenter
    ray = Ray cameraCenter rayDirection
