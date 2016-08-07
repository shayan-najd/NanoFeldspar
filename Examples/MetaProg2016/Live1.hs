module Examples.MetaProg2016.Live1 where

import IL
import BackEnd.Compiler

inc :: Prg (Word32 -> Word32)
inc = Fun (\ x -> Exp (Add x (LitI 1)))

-- GHCi> :t compile
-- GHCi> putStrLn (compile inc)
