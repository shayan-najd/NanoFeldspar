{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RankNTypes,FlexibleInstances,TypeFamilies,
             GADTs,TypeOperators #-}
module Examples.MetaProg2016.NBE.Simple where

import Control.Monad.State

type a :*: b = (a , b)

data Exp a where
  Lit :: Int -> Exp Int
  Add :: Exp Int -> Exp Int -> Exp Int
  Abs :: (Exp a -> Exp b) -> Exp (a -> b)
  App :: Exp (a -> b) -> Exp a -> Exp b
  Tup :: Exp a -> Exp b -> Exp (a :*: b)
  Fst :: Exp (a :*: b) -> Exp a
  Snd :: Exp (a :*: b) -> Exp b
  Val :: a -> Exp a
  Var :: String -> Exp a

eval :: Exp a -> a
eval e
  = case e of
      Lit i     -> i
      Add m n   -> (eval m) + (eval n)

      Abs n     -> \ x -> eval (n (Val x))
      App l m   -> (eval l) (eval m)

      Tup m n   -> (eval m , eval n)
      Fst l     -> fst (eval l)
      Snd l     -> snd (eval l)

      Val v     -> v
      Var _     -> error "Impossible!"

type family InT a
type instance InT (Exp a)   = a
type instance InT (a ->  b) = InT a ->  InT b
type instance InT (a :*: b) = InT a :*: InT b

class Reify a where
  reify :: a -> Exp (InT a)
instance Reify (Exp a) where
  reify u = u
instance (Reflect a , Reify b) => Reify (a -> b) where
  reify u = Abs (\ x -> reify (u (reflect x)))
instance (Reify a , Reify b) => Reify (a :*: b) where
  reify u = Tup (reify (fst u)) (reify (snd u))

class Reflect a where
  reflect :: Exp (InT a) -> a
instance Reflect (Exp a) where
  reflect l = l
instance (Reify a , Reflect b) => Reflect (a -> b) where
  reflect l = \ x -> reflect (App l (reify x))
instance (Reflect a , Reflect b) => Reflect (a :*: b) where
  reflect l = (reflect (Fst l) , reflect (Snd l))

instance Show (Exp a) where
  show e = evalState (showExpM e) 0

showExp1 :: String -> Exp a -> State Int String
showExp1 s0 m = do s1 <- showExpM m
                   return (s0 ++" (" ++ s1 ++ ")")


showExp2 :: String -> Exp a -> Exp b -> State Int String
showExp2 s0 m n = do s1 <- showExpM m
                     s2 <- showExpM n
                     return (s0 ++" (" ++ s1 ++ ") (" ++ s2 ++ ")")

showExp3 :: String -> Exp a -> Exp b -> Exp c -> State Int String
showExp3 s0 l m n = do s1 <- showExpM l
                       s2 <- showExpM m
                       s3 <- showExpM n
                       return (s0 ++" (" ++ s1 ++ ") (" ++
                                            s2 ++ ") (" ++ s3 ++ ")")

showExpM :: Exp a -> State Int String
showExpM (Lit i)     = return ("Lit" ++ show i)
showExpM (Add m n)   = showExp2 "Add" m n
showExpM (Abs n)     = do s <- showExpFM n
                          return ("Abs (" ++ s ++ ")")
showExpM (App l m)   = showExp2 "App" l m

showExpM (Tup m n)   = showExp2 "Tup" m n
showExpM (Fst l)     = showExp1 "Fst" l
showExpM (Snd l)     = showExp1 "Snd" l

showExpM (Val _)     = return ("Val _")
showExpM (Var s)     = return s

showExpFM :: (Exp a -> Exp b) -> State Int String
showExpFM f = do i <- get
                 put (i + 1)
                 let x = "x" ++ show i
                 s <- showExpM (f (Var x))
                 return ("\\ " ++ x ++ " -> (" ++ s ++ ")")
