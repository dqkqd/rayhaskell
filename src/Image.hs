module Image (Image (Image), writeImage) where

import Control.Monad (forM_)
import System.IO (Handle, hPrint, hPutStrLn)
import Text.Printf (hPrintf)

import Color (Color)

data Image = Image
  { imageWidth :: Int
  , imageHeight :: Int
  , imageData :: [[Color]]
  }

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
