module Examples.MetaProg2016.Live14 where

import FrontEnd.Core

not :: Exp Bool -> Exp Bool
not x = x ? (FalseE , TrueE)
