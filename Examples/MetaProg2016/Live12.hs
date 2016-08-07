module Examples.MetaProg2016.Live12 where

import FrontEnd.Core

not :: Exp Bool -> Exp Bool
not x = Cnd x (LitB False) (LitB True)
