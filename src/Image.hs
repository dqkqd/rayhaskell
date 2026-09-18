module Image (Image (Image), writeImage) where

import Control.Monad (forM_)
import System.IO (Handle, hPrint, hPutStrLn)
import Text.Printf (hPrintf)

import Color (Color)

-- $setup
-- >>> import Color (color)
-- >>> import System.IO (stdout)

data Image = Image
  { imageWidth :: Int
  , imageHeight :: Int
  , imageData :: [[Color]]
  }

-- | Write an image out to a handle
--
--
-- >>> colors = [[color 0.1 0.2 0.3, color 0.2 0.3 0.4, color 0.3 0.4 0.5], [color 0.4 0.5 0.6, color 0.5 0.6 0.7, color 0.6 0.7 0.8]]
-- >>> colors
-- [[25 51 76,51 76 102,76 102 127],[102 127 153,127 153 179,153 179 204]]
-- >>> image = Image 3 2 colors
-- >>> writeImage image stdout
-- P3
-- 3 2
-- 255
-- 25 51 76
-- 51 76 102
-- 76 102 127
-- 102 127 153
-- 127 153 179
-- 153 179 204
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
