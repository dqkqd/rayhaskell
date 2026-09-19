module Camera (
  render,
  CameraConfig (
    CameraConfig,
    ratioConfig,
    imageWidthConfig,
    samplesPerPixelConfig,
    maxDepthConfig,
    fieldOfViewConfig,
    lookFromConfig,
    lookAtConfig,
    viewUpConfig
  ),
  createCamera,
) where

import Control.Monad (forM_, replicateM)
import Control.Monad.Random (
  Rand,
  StdGen,
  evalRandIO,
 )
import System.ProgressBar

import Color (Color (C), black)
import Hit.Hittable (hitMany)
import Interval (Interval (Interval))
import Material.Impl (materialScatter)
import Material.Material (Scatter (scatterAttenuation, scatterRay))
import Point (Point, (.+^), (.-.), (.-^))
import Ray (Ray (Ray, rayDirection, rayOrigin))
import System.IO (Handle, hPrint, hPutStrLn)
import Text.Printf (hPrintf)
import Vec3 (Vec3 (V3), cross, lengthSquare, randomVecR, unit, (^*), (^/))
import World (WorldObject)

-- | camera configuration
data CameraConfig = CameraConfig
  { ratioConfig :: Double
  , imageWidthConfig :: Int
  , samplesPerPixelConfig :: Int
  , maxDepthConfig :: Int
  , fieldOfViewConfig :: Double
  , lookFromConfig :: Point
  , lookAtConfig :: Point
  , viewUpConfig :: Vec3 Double
  }

-- | Actual camera, this can only be created from a configuration
data Camera = Camera
  { imageWidth :: Int
  , imageHeight :: Int
  , center :: Point
  , samplesPerPixel :: Int
  , maxDepth :: Int
  , fieldOfView :: Double
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
    , maxDepthConfig = maxDepth
    , fieldOfViewConfig = fieldOfView
    , lookFromConfig = lookFrom
    , lookAtConfig = lookAt
    , viewUpConfig = viewUp
    } =
    Camera
      { imageWidth = imageWidth
      , imageHeight = imageHeight
      , samplesPerPixel = samplesPerPixel
      , maxDepth = maxDepth
      , center = center
      , pixel00Location = pixel00Location
      , pixelDeltaU = pixelDeltaU
      , pixelDeltaV = pixelDeltaV
      , fieldOfView = fieldOfView
      }
   where
    imageHeight = floor (fromIntegral imageWidth / ratio)

    focalLength = sqrt $ lengthSquare (lookFrom .-. lookAt)
    theta = pi / 180 * fieldOfView
    h = tan (theta / 2)
    viewportHeight = 2 * h * focalLength
    viewportWidth = viewportHeight * (fromIntegral imageWidth / fromIntegral imageHeight)
    center = lookFrom

    w = unit (lookFrom .-. lookAt)
    u = unit (viewUp `cross` w)
    v = w `cross` u

    viewportU = u ^* viewportWidth
    viewportV = (-v) ^* viewportHeight

    pixelDeltaU = viewportU ^/ fromIntegral imageWidth
    pixelDeltaV = viewportV ^/ fromIntegral imageHeight

    viewportUpperLeft =
      center
        .-^ (w ^* focalLength)
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
             , maxDepth = maxDepth
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
        colors <-
          evalRandIO $
            replicateM samplesPerPixel $ do
              ray <- sampleRay camera i j
              rayColor maxDepth world ray
        let averageColor = sum colors / fromIntegral (length colors)
        hPrint h averageColor

sampleRay :: Camera -> Int -> Int -> Rand StdGen Ray
sampleRay camera i j = do
  V3 x y _ <- randomVecR (Interval (-0.5) 0.5)
  let
    pixelSample =
      pixel00Location camera
        .+^ (pixelDeltaU camera ^* (x + fromIntegral i))
        .+^ (pixelDeltaV camera ^* (y + fromIntegral j))
    rayDirection = pixelSample .-. center camera
  return Ray{rayOrigin = center camera, rayDirection = rayDirection}

-- | Render color from a ray hitting the world
rayColor ::
  Int -> -- the number of remaning depth
  [WorldObject] ->
  Ray ->
  Rand StdGen Color
rayColor 0 _ _ = return black
rayColor depth objects ray = do
  let
    record = hitMany objects ray (Interval 0.001 (1 / 0))
    color = case record of
      Just h -> do
        scatter <- materialScatter h
        case scatter of
          Nothing -> return black
          Just s -> do
            nextColor <- rayColor (depth - 1) objects (scatterRay s)
            return (nextColor * scatterAttenuation s)
      Nothing -> return (backgroundColor ray)

  color

-- | Background default color
backgroundColor :: Ray -> Color
backgroundColor (Ray _ direction) = C (V3 1 1 1 ^* (1 - a) + V3 0.5 0.7 1.0 ^* a)
 where
  V3 _ y _ = unit direction
  a = 0.5 * (y + 1.0)
