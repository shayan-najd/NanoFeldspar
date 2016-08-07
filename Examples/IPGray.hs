module Examples.IPGray where

import FrontEnd.Core

rgbToGray :: Exp Word32 -> Exp Word32 -> Exp Word32 -> Exp Word32
rgbToGray r g b = ((r * 30) + (g * 59) + (b * 11)) `divE` 100

ipgray :: Vec (Exp Word32) -> Vec (Exp Word32)
ipgray (Vec l f) =
  Vec (divE l 3)
      (\ i -> let j = i * 3
              in rgbToGray (f j)
                           (f (j + 1))
                           (f (j + 2)))
