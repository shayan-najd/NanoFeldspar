module Examples.IPBW where

import FrontEnd.Core

ipbw :: Vec (Exp Word32) -> Vec (Exp Word32)
ipbw = fmap (\ x -> (x <. 135) ? (1 , 0))
