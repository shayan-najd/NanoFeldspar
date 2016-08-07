module Examples.MetaProg2016.Live3 where

import FrontEnd.Core

inc :: Prg (Word32 -> Word32)
inc = Fun (\ x -> Exp (x + 1))
