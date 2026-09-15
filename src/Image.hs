module Image (writeImage, image) where

import Control.Monad (forM_)
import System.IO (Handle, hPutStrLn)
import Text.Printf (hPrintf)

import Color (Color, formatColor)
import Vec3 (Vec3 (Vec3))

data Image = Image
  { imageWidth :: Int
  , imageHeight :: Int
  , imageData :: [[Color]]
  }
  deriving (Eq)

writeImage ::
  Image -> -- the image
  Handle -> -- the file handle
  IO ()
writeImage img h = do
  hPutStrLn h "P3"
  hPrintf h "%d %d\n" (imageWidth img) (imageHeight img)
  hPutStrLn h "255"

  forM_ (imageData img) $ \r ->
    forM_ r $ \c ->
      hPutStrLn h $ formatColor c

image :: Image
image = Image width height colors
 where
  width = 256
  height = 256
  colors =
    [ [ color i j
      | i <- [(0 :: Int) .. width - 1]
      ]
    | j <- [(0 :: Int) .. height - 1]
    ]

  color i j = Vec3 r g b
   where
    r = fromIntegral i / fromIntegral (width - 1)
    g = fromIntegral j / fromIntegral (height - 1)
    b = 0.0
