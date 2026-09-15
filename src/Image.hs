module Image (writeImage, image) where

import Control.Monad (forM_)
import System.IO (Handle, hPutStrLn)
import Text.Printf (hPrintf)

import Color (Color, formatColor)
import Vec3 (fromTuple)

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

  color i j = fromTuple (r, g, b)
   where
    r :: Double = fromIntegral i / fromIntegral (width - 1)
    g :: Double = fromIntegral j / fromIntegral (height - 1)
    b :: Double = 0.0
