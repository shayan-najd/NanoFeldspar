{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts   #-}
module IL (module Data.Word,
           module Data.Array,
           Ary,
           Exp(..),
           Prg(..),
           STyp(..),
           Type(..),
           typOf) where

import Data.Word
import Data.Array
import Control.Monad.State

type Ary a = Array Word32 a

-- Expressions
data Exp a where
  Var   :: String -> Exp a
  LitI  :: Word32 -> Exp Word32
  Add   :: Exp Word32 -> Exp Word32 -> Exp Word32
  Sub   :: Exp Word32 -> Exp Word32 -> Exp Word32
  Mul   :: Exp Word32 -> Exp Word32 -> Exp Word32
  Div   :: Exp Word32 -> Exp Word32 -> Exp Word32
  Mod   :: Exp Word32 -> Exp Word32 -> Exp Word32
  Sqrt  :: Exp Word32 -> Exp Word32
  Eql   :: Exp Word32 -> Exp Word32 -> Exp Bool
  Ltd   :: Exp Word32 -> Exp Word32 -> Exp Bool
  Tup   :: (Type a, Type b) => Exp a -> Exp b -> Exp (a , b)
  Fst   :: Type (a , b) => Exp (a , b) -> Exp a
  Snd   :: Type (a , b) => Exp (a , b) -> Exp b
  Ary   :: Type a => Exp Word32 -> (Exp Word32 -> Exp a) -> Exp (Ary a)
  Len   :: Type a => Exp (Ary a) -> Exp Word32
  Ind   :: Exp (Ary a) -> Exp Word32 -> Exp a
  LitB  :: Bool -> Exp Bool
  Cnd   :: Exp Bool -> Exp a -> Exp a -> Exp a

data Prg a where
  Exp :: Type a => Exp a -> Prg a
  Fun :: Type a => (Exp a -> Prg b) -> Prg (a -> b)

data STyp a where
  SWrd :: STyp Word32
  SBol :: STyp Bool
  STup :: (Type a,Type b) => STyp a -> STyp b -> STyp (a , b)
  SAry :: Type a => STyp a -> STyp (Ary a)

class Type a where {typ :: STyp a}


instance Type Word32 where
  typ = SWrd
instance Type Bool where
  typ = SBol
instance (Type a, Type b) => Type (a , b) where
  typ = STup typ typ
instance Type a => Type (Ary a) where
  typ = SAry typ

typOf :: Type a => c a -> STyp a
typOf _ = typ

deriving instance Show (STyp t)

instance Show (Prg a) where
  show p = evalState (showPrgM p) 0

showPrgM :: Prg a -> State Word32 String
showPrgM (Exp e) = showExpM e
showPrgM (Fun f) = do i <- get
                      put (i + 1)
                      showPrgM (f (Var ("x" ++ show i)))

instance Show (Exp a) where
  show e = evalState (showExpM e) 0


showExp1 :: String -> Exp a -> State Word32 String
showExp1 s0 m = do s1 <- showExpM m
                   return (s0 ++" (" ++ s1 ++ ")")


showExp2 :: String -> Exp a -> Exp b -> State Word32 String
showExp2 s0 m n = do s1 <- showExpM m
                     s2 <- showExpM n
                     return (s0 ++" (" ++ s1 ++ ") (" ++ s2 ++ ")")

showExp3 :: String -> Exp a -> Exp b -> Exp c -> State Word32 String
showExp3 s0 l m n = do s1 <- showExpM l
                       s2 <- showExpM m
                       s3 <- showExpM n
                       return (s0 ++" (" ++ s1 ++ ") (" ++
                                            s2 ++ ") (" ++ s3 ++ ")")

showExpM :: Exp a -> State Word32 String
showExpM (LitI i)       = return ("LitI " ++ show i)
showExpM (Add t1 t2)    = showExp2 "Add" t1 t2
showExpM (Sub t1 t2)    = showExp2 "Sub" t1 t2
showExpM (Mul t1 t2)    = showExp2 "Mul" t1 t2
showExpM (Div t1 t2)    = showExp2 "Div" t1 t2
showExpM (Eql t1 t2)    = showExp2 "Eql" t1 t2
showExpM (Mod t1 t2)    = showExp2 "Mod" t1 t2
showExpM (Sqrt t1)      = showExp1 "Sqrt" t1
showExpM (Ltd t1 t2)    = showExp2 "Ltd" t1 t2
showExpM (LitB b)       = return ("LitB " ++ show b)
showExpM (Cnd t1 t2 t3) = showExp3 "Cnd" t1 t2 t3
showExpM (Tup t1 t2)    = showExp2 "Tup" t1 t2
showExpM (Fst t)        = showExp1 "Fst" t
showExpM (Snd t)        = showExp1 "Snd" t
showExpM (Ary t1 t2)    = do s1 <- showExpM  t1
                             s2 <- showExpFM t2
                             return ("Ary (" ++ s1 ++ ") ("
                                             ++ s2 ++ ")")
showExpM (Len t)        = showExp1 "Len" t
showExpM (Ind t1 t2)    = showExp2 "Ind" t1 t2
showExpM (Var t)        = return t

showExpFM :: (Exp a -> Exp b) -> State Word32 String
showExpFM f = do i <- get
                 put (i + 1)
                 let x = "x" ++ show i
                 s <- showExpM (f (Var x))
                 return ("\\ " ++ x ++ " -> (" ++ s ++ ")")
