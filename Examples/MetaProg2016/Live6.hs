module Examples.MetaProg2016.Live6 where

import FrontEnd.Core
import qualified BackEnd.Compiler as BackEnd

avg :: Prg (Word32 -> Word32 -> Word32)
avg = fromFun2 (\ x y -> (x + y) `divE` 2)

c :: String
c = BackEnd.compile avg

fromFun1 :: (Exp Word32 -> Exp Word32) ->
            Prg (Word32 -> Word32)
fromFun1 f = Fun (\ x -> Exp (f x))

fromFun2 :: (Exp Word32 -> Exp Word32 -> Exp Word32) ->
            Prg (Word32 -> Word32 -> Word32)
fromFun2 f = Fun (\ x -> fromFun1 (f x))
