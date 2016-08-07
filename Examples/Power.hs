module Examples.Power where

import FrontEnd.Core

power :: Int -> Exp Word32 -> Exp Word32
power n x = (x ==. 0) ?
  (0 ,
   if n == 0 then
     1
   else if even n then
     sqr (power (div n 2) x)
   else
     x * power (n-1) x)

sqr ::  Exp Word32 -> Exp Word32
sqr y = y * y
