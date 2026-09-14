module Main (main) where

import GHC.IO.Handle.FD (withFile)
import GHC.IO.IOMode (IOMode (WriteMode))
import Image (image, writeImage)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  let output = case args of
        [filename] -> filename
        _ -> "output.ppm"
  withFile output WriteMode $ writeImage image
