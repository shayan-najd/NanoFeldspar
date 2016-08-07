module Examples.MetaProg2016.Live8 where

import FrontEnd.Core

type Point = Exp (Word32 , Word32)

distance :: Point -> Point -> Exp Word32
distance p p'
  = let x  = Fst p  ; y  = Snd p
        x' = Fst p' ; y' = Snd p'
        dx = x' - x ; dy = y' - y
    in sqrtE ((dx * dx) + (dy * dy))
