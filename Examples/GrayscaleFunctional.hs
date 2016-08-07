module Examples.GrayscaleFunctional where

import FrontEnd.Core
import Examples.IPLib

mapImage :: (Exp Word32 -> Exp Word32 -> Exp Word32 -> Pixel) -> Image -> Image
mapImage f image = mkImage (heightImage image)
                           (widthImage  image)
                           (\ i j -> let p = getPixel image i j
                                         r = red   p
                                         g = green p
                                         b = blue  p
                                     in  f r g b)

grayscale :: Image -> Image
grayscale img =
  mapImage (\ r g b -> let q = ((30 * r) + (59 * g) + (11 * b)) `divE` 100
                       in  mkPixel q q q)
           img

run :: IO ()
run = compileImageProcessor "grayscale" grayscale

-- :! ./Examples/C/grayscale ./Examples/C/Image.ppm ./Examples/C/Image.pgm 3
