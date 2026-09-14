module Image (writeImage, image) where

import Control.Monad (forM_)
import System.IO (Handle, hPrint, hPutStrLn)
import Text.Printf (hPrintf)

data Image = Image
  { imageWidth :: Int
  , imageHeight :: Int
  , imageData :: [[Color]]
  }
  deriving (Eq)

data Color = Color Int Int Int
  deriving (Eq)

instance Show Color where
  show (Color r g b) = show r ++ " " ++ show g ++ " " ++ show b

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
      hPrint h c

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

  color i j = Color ir ig ib
   where
    r :: Double = fromIntegral i / fromIntegral (width - 1)
    g :: Double = fromIntegral j / fromIntegral (height - 1)
    b :: Double = 0.0

    ir = floor (255.999 * r)
    ig = floor (255.999 * g)
    ib = floor (255.999 * b)
