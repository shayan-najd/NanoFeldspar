module Examples.MetaProg2016.Live10 where

import FrontEnd.Core

reverseE :: Exp (Ary Word32) -> Exp (Ary Word32)
reverseE l = Ary (Len l) (\ x -> Ind l (Len l - 1 - x))

revRev :: Exp (Ary Word32) -> Exp (Ary Word32)
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
  Wrd v12;
  AryWrd v11;
  Wrd v10;
  Wrd v9;
  AryWrd v8;
  Wrd v7;
  Wrd v6;
  AryWrd v5;
  Wrd v4;
  Wrd v3;
  AryWrd v2;
  Wrd v1;
  v4 = len (v0);
  v5 = newAry (Wrd, v4);
  v6 = 0u;
  while (ltd (v6, v4))
  {
    v5 = setAry (v5, v6, ind (v0, sub (sub (len (v0), 1u), v6)));
    v6 = add (v6, 1u);
  }
  v1 = len (v5);
  v2 = newAry (Wrd, v1);
  v3 = 0u;
  while (ltd (v3, v1))
  {
    v7 = len (v0);
    v8 = newAry (Wrd, v7);
    v9 = 0u;
    while (ltd (v9, v7))
    {
      v8 = setAry (v8, v9, ind (v0, sub (sub (len (v0), 1u), v9)));
      v9 = add (v9, 1u);
    }
    v10 = len (v0);
    v11 = newAry (Wrd, v10);
    v12 = 0u;
    while (ltd (v12, v10))
    {
      v11 = setAry (v11, v12, ind (v0, sub (sub (len (v0), 1u), v12)));
      v12 = add (v12, 1u);
    }
    v2 = setAry (v2, v3, ind (v8, sub (sub (len (v11), 1u), v3)));
    v3 = add (v3, 1u);
  }
  return v2;
}
-}
