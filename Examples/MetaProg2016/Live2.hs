module Examples.MetaProg2016.Live2 where

import IL
import BackEnd.Evaluator

inc :: Prg (Word32 -> Word32)
inc = Fun (\ x -> Exp (Add x (LitI 1)))

-- GHCi> :t evaluate
-- GHCi> evaluate inc 41
