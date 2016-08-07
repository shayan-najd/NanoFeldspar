module Examples.MetaProg2016.Live5 where

import FrontEnd.Core

avg :: Prg (Word32 -> Word32 -> Word32)
avg = Fun (\ x -> Fun (\ y -> Exp ((x + y) `divE` 2)))
