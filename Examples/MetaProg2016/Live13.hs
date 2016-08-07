module Examples.MetaProg2016.Live13 where

import FrontEnd.Core

{-
not :: Bool -> Bool
not True  = False
not False = True
-}

{-
not :: Bool -> Bool
not x = if x
        then False
        else True
-}

not :: Bool -> Bool
not x = case x
          True  -> False
          False -> True
