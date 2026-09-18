module Camera (
  render,
  CameraConfig (
    CameraConfig,
    ratioConfig,
    imageWidthConfig,
    samplesPerPixelConfig
  ),
  createCamera,
) where

import System.ProgressBar

import Color (Color (C))
import Control.Monad (forM_, replicateM)
import Control.Monad.Random (
  MonadRandom (getRandom),
  Rand,
  StdGen,
  evalRandIO,
 )
import Hit (hitMany, hitRecordColor)
import Interval (defaultInterval)
import Point (Point, point, (.+^), (.-.), (.-^))
import Ray (Ray (Ray))
import System.IO (Handle, hPrint, hPutStrLn)
import Text.Printf (hPrintf)
import Vec3 (Vec3 (V3), unit, (^*))
import World (WorldObject)

-- | camera configuration
data CameraConfig = CameraConfig
  { ratioConfig :: Double
  , imageWidthConfig :: Int
  , samplesPerPixelConfig :: Int
  }

-- | Actual camera, this can only be created from a configuration
data Camera = Camera
  { imageWidth :: Int
  , imageHeight :: Int
  , center :: Point
  , samplesPerPixel :: Int
  , pixel00Location :: Point
  , pixelDeltaU :: Vec3 Double
  , pixelDeltaV :: Vec3 Double
  }

-- | Create a camera
createCamera :: CameraConfig -> Camera
createCamera
  CameraConfig
    { ratioConfig = ratio
    , imageWidthConfig = imageWidth
    , samplesPerPixelConfig = samplesPerPixel
    } =
    Camera
      { imageWidth = imageWidth
      , imageHeight = imageHeight
      , samplesPerPixel = samplesPerPixel
      , center = point 0 0 0
      , pixel00Location = pixel00Location
      , pixelDeltaU = pixelDeltaU
      , pixelDeltaV = pixelDeltaV
      }
   where
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
render :: Camera -> [WorldObject] -> Handle -> IO ()
render
  camera@( Camera
             { imageWidth = imageWidth
             , imageHeight = imageHeight
             , samplesPerPixel = samplesPerPixel
             }
           )
  world
  h = do
    hPutStrLn h "P3"
    hPrintf h "%d %d\n" imageWidth imageHeight
    hPutStrLn h "255"

    pb <- newProgressBar defStyle 10 (Progress 0 imageHeight ())
    forM_ [0 .. imageHeight - 1] $ \j -> do
      incProgress pb 1
      forM_ [0 .. imageWidth - 1] $ \i -> do
        rays <- evalRandIO (replicateM samplesPerPixel (sampleRay camera i j))
        let
          color =
            sum
              (map (rayColor world) rays)
              / fromIntegral samplesPerPixel
        hPrint h color

sampleRay :: Camera -> Int -> Int -> Rand StdGen Ray
sampleRay camera i j = do
  V3 x y _ <- sampleSquare
  let
    pixelSample =
      pixel00Location camera
        .+^ (pixelDeltaU camera ^* (x + fromIntegral i))
        .+^ (pixelDeltaV camera ^* (y + fromIntegral j))
    rayDirection = pixelSample .-. center camera
  return (Ray (center camera) rayDirection)

sampleSquare :: Rand StdGen (Vec3 Double)
sampleSquare = do
  x <- getRandom
  y <- getRandom
  return (V3 (x - 0.5) (y - 0.5) 0)

-- | Render color from a ray hitting the world
rayColor :: [WorldObject] -> Ray -> Color
rayColor objects ray = maybe (backgroundColor ray) hitRecordColor record
 where
  record = hitMany objects ray defaultInterval

-- | Background default color
backgroundColor :: Ray -> Color
backgroundColor (Ray _ direction) = C (V3 1 1 1 ^* (1 - a) + V3 0.5 0.7 1.0 ^* a)
 where
  V3 _ y _ = unit direction
  a = 0.5 * (y + 1.0)
