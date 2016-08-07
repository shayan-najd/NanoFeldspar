module Examples.MetaProg2016.Live11 where

import FrontEnd.Core

reverseE :: Vec (Exp Word32) -> Vec (Exp Word32)
reverseE u = Vec (len u) (\ x -> ind u (len u - 1 - x))

revRev :: Vec (Exp Word32) -> Vec (Exp Word32)
revRev = reverseE . reverseE

{-
GHCi> putStrLn (compile reverseE)

AryWrd func (AryWrd v0)
{
  Wrd v3;
  AryWrd v2;
  Wrd v1;
  v1 = len (v0);
  v2 = newAry (Wrd, v1);
  v3 = 0u;
  while (ltd (v3, v1))
  {
    v2 = setAry (v2, v3, ind (v0, sub (sub (len (v0), 1u), v3)));
    v3 = add (v3, 1u);
  }
  return v2;
}

GHCi> putStrLn (compile revRev)

AryWrd func (AryWrd v0)
{
  Wrd v3;
  AryWrd v2;
  Wrd v1;
  v1 = len (v0);
  v2 = newAry (Wrd, v1);
  v3 = 0u;
  while (ltd (v3, v1))
  {
    v2 = setAry (v2, v3, ind (v0, sub (sub (len (v0), 1u), sub (sub (len (v0), 1u), v3))));
    v3 = add (v3, 1u);
  }
  return v2;
}

-}
