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
    viewUpConfig,
    defocusAngleConfig,
    focusDistConfig
  ),
  createCamera,
) where

import Control.Monad (forM, forM_, replicateM)
import Control.Monad.Random (
  Rand,
  StdGen,
  evalRand,
  newStdGen,
 )
import System.ProgressBar

import Color (Color (C), black)
import Control.Parallel.Strategies (parListChunk, rdeepseq, using)
import Hit.Hittable (hitMany)
import Interval (Interval (Interval))
import Material.Impl (materialScatter)
import Material.Material (Scatter (scatterAttenuation, scatterRay))
import Point (Point, (.+^), (.-.), (.-^))
import Ray (Ray (Ray, rayDirection, rayOrigin))
import System.IO (Handle, hPrint, hPutStrLn)
import Text.Printf (hPrintf)
import Vec3 (
  Vec3 (V3),
  cross,
  randomInUnitDisk,
  randomVecR,
  unit,
  (^*),
  (^/),
 )
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
  , defocusAngleConfig :: Double
  , focusDistConfig :: Double
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
  , defocusAngle :: Double
  , defocusDiskU :: Vec3 Double
  , defocusDiskV :: Vec3 Double
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
    , defocusAngleConfig = defocusAngle
    , focusDistConfig = focusDist
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
      , defocusAngle = defocusAngle
      , defocusDiskU = defocusDiskU
      , defocusDiskV = defocusDiskV
      }
   where
    degreeToRadian :: Double -> Double
    degreeToRadian = (* (pi / 180))

    imageHeight = floor (fromIntegral imageWidth / ratio)

    theta = degreeToRadian fieldOfView
    h = tan (theta / 2)
    viewportHeight = 2 * h * focusDist
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
        .-^ (w ^* focusDist)
        .-^ (viewportU / 2)
        .-^ (viewportV / 2)

    pixel00Location = viewportUpperLeft .+^ (0.5 * pixelDeltaU) .+^ (0.5 * pixelDeltaV)

    defocusRadius = focusDist * tan (degreeToRadian (defocusAngle / 2))
    defocusDiskU = u ^* defocusRadius
    defocusDiskV = v ^* defocusRadius

-- | Render list of WorldObject into Image
render :: Camera -> [WorldObject] -> Handle -> IO ()
render
  camera@( Camera
             { imageWidth = imageWidth
             , imageHeight = imageHeight
             }
           )
  world
  h = do
    hPutStrLn h "P3"
    hPrintf h "%d %d\n" imageWidth imageHeight
    hPutStrLn h "255"

    pb <- newProgressBar defStyle 10 (Progress 0 imageHeight ())
    rowSeeds <- forM [0 .. imageHeight - 1] (const newStdGen)
    let rows =
          [ evalRand (renderRow camera world j) seed
          | (j, seed) <- zip [0 ..] rowSeeds
          ]
            `using` parListChunk 8 rdeepseq

    forM_ rows $ \row -> do
      incProgress pb 1
      forM_ row $ \c -> do
        hPrint h c

-- | Generate row in parallel
renderRow ::
  Camera ->
  [WorldObject] ->
  Int ->
  Rand StdGen [Color]
renderRow camera objects j = mapM pixel [0 .. imageWidth camera - 1]
 where
  pixel i = do
    colors <-
      replicateM (samplesPerPixel camera) $ do
        ray <- sampleRay camera i j
        rayColor (maxDepth camera) objects ray
    let averageColor = sum colors / fromIntegral (samplesPerPixel camera)
    return averageColor

sampleRay :: Camera -> Int -> Int -> Rand StdGen Ray
sampleRay camera i j = do
  V3 x y _ <- randomVecR (Interval (-0.5) 0.5)
  defocusedAngle <- defocusDiskSample camera
  let
    pixelSample =
      pixel00Location camera
        .+^ (pixelDeltaU camera ^* (x + fromIntegral i))
        .+^ (pixelDeltaV camera ^* (y + fromIntegral j))
    rayOrigin =
      if defocusAngle camera <= 0
        then center camera
        else defocusedAngle
    rayDirection = pixelSample .-. rayOrigin

  return Ray{rayOrigin = rayOrigin, rayDirection = rayDirection}

-- | Return a random point in the camera defocus disk
defocusDiskSample :: Camera -> Rand StdGen Point
defocusDiskSample camera = do
  V3 u v _ <- randomInUnitDisk
  return $
    center camera
      .+^ (defocusDiskU camera ^* u)
      .+^ (defocusDiskV camera ^* v)

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
