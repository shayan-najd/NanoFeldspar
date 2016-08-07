{-# OPTIONS_GHC -Wall -fno-warn-orphans  #-}
module BackEnd.C where

import Data.Word

type Var = (String , Typ)

data Func = Func Typ String [Var] [Stmt]

data Stmt =
   If  Exp [Stmt] [Stmt]
 | Whl Exp [Stmt]
 | Assign String Exp
 | Declare Var
 | Return  Exp

data Exp =
   Var String
 | Wrd Word32
 | App String [Exp]
 deriving Eq

data Typ = TWrd | TBol | TTup Typ Typ | TAry Typ
 deriving (Show,Eq)
