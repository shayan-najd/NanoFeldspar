module Examples.MetaProg2016.Live7 where

import FrontEnd.Core
import qualified FrontEnd.Core    as FrontEnd

avg :: Exp Word32 -> Exp Word32 -> Exp Word32
avg x y = (x + y) `divE` 2

c :: String
c = FrontEnd.compile avg
