module Examples.MetaProg2016.Live4 where

import FrontEnd.Core

inc :: Prg (Word32 -> Word32)
inc = fromFun1 (\ x -> x + 1)

fromFun1 :: (Exp Word32 -> Exp Word32) ->
            Prg (Word32 -> Word32)
fromFun1 f = Fun (\ x -> Exp (f x))
