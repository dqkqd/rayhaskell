module Camera (
  render,
  CameraConfig (CameraConfig, ratioConfig, imageWidthConfig),
  createCamera,
) where

import Color (Color (C))
import Hit (hitMany, hitRecordColor)
import Image (Image (Image))
import Interval (defaultInterval)
import Point (Point, point, (.+^), (.-.), (.-^))
import Ray (Ray (Ray))
import Vec3 (Vec3 (V3), unit, (^*))
import World (WorldObject)

-- | camera configuration
data CameraConfig = CameraConfig
  { ratioConfig :: Double
  , imageWidthConfig :: Int
  }

-- | Actual camera, this can only be created from a configuration
data Camera = Camera
  { imageWidth :: Int
  , imageHeight :: Int
  , center :: Point
  , pixel00Location :: Point
  , pixelDeltaU :: Vec3 Double
  , pixelDeltaV :: Vec3 Double
  }

-- | Create a camera
createCamera :: CameraConfig -> Camera
createCamera config =
  Camera
    { imageWidth = imageWidth
    , imageHeight = imageHeight
    , center = point 0 0 0
    , pixel00Location = pixel00Location
    , pixelDeltaU = pixelDeltaU
    , pixelDeltaV = pixelDeltaV
    }
 where
  ratio = ratioConfig config
  imageWidth = imageWidthConfig config
  imageHeight = floor (fromIntegral imageWidth / ratio)

  focalLength :: Double = 1.0
  viewportHeight :: Double = 2.0
  viewportWidth = viewportHeight * (fromIntegral imageWidth / fromIntegral imageHeight)
  cameraCenter = point 0 0 0

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

-- | Render list of WorldObject into Image
render :: Camera -> [WorldObject] -> Image
render
  ( Camera
      { imageWidth = imageWidth
      , imageHeight = imageHeight
      , center = center
      , pixel00Location = pixel00Location
      , pixelDeltaU = pixelDeltaU
      , pixelDeltaV = pixelDeltaV
      }
    )
  world = Image imageWidth imageHeight colors
   where
    colors =
      [ [ color i j
        | i <- [(0 :: Int) .. imageWidth - 1]
        ]
      | j <- [(0 :: Int) .. imageHeight - 1]
      ]

    color :: Int -> Int -> Color
    color i j = rayColor ray world
     where
      pixelCenter =
        pixel00Location
          .+^ (pixelDeltaU * fromIntegral i)
          .+^ (pixelDeltaV * fromIntegral j)
      rayDirection = pixelCenter .-. center
      ray = Ray center rayDirection

-- | Render color from a ray hitting the world
rayColor :: Ray -> [WorldObject] -> Color
rayColor ray objects = maybe (backgroundColor ray) hitRecordColor record
 where
  record = hitMany objects ray defaultInterval

-- | Background default color
backgroundColor :: Ray -> Color
backgroundColor (Ray _ direction) = C (V3 1 1 1 ^* (1 - a) + V3 0.5 0.7 1.0 ^* a)
 where
  V3 _ y _ = unit direction
  a = 0.5 * (y + 1.0)
