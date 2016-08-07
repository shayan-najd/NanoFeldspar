module Examples.MetaProg2016.Live9 where

import FrontEnd.Core

type Point = (Exp Word32 , Exp Word32)

distance :: Point -> Point -> Exp Word32
distance (x,y) (x',y')
  = let dx = x' - x ; dy = y' - y
    in  sqrtE ((dx * dx) + (dy * dy))


{-
GHCi> evaluate distance (0,0) (3,4)
5
-}

{-
GHCi> expand (compile distance)
[...]
typedef struct {unsigned int fst; unsigned int snd;} TupWrdWrd;

unsigned int func (TplWrdWrd v1, TplWrdWrd v0)
{
  return
   ((unsigned int)sqrtf(
      ((((v1.fst) - (v0.fst)) * ((v1.fst) - (v0.fst)))
       +
       (((v1.snd) - (v0.snd)) * ((v1.snd) - (v0.snd))))));
}
-}
