{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE RankNTypes,FlexibleInstances,TypeFamilies,
             GADTs,TypeOperators,ScopedTypeVariables #-}
module Examples.MetaProg2016.NBE.Monadic where

import Control.Monad
import Control.Applicative
import Control.Monad.State

type a :*: b = (a , b)
type a :+: b = Either a b

data Exp a where
  Lit :: Int -> Exp Int
  Add :: Exp Int -> Exp Int -> Exp Int
  Abs :: (Exp a -> Exp b) -> Exp (a -> b)
  App :: Exp (a -> b) -> Exp a -> Exp b
  Tup :: Exp a -> Exp b -> Exp (a :*: b)
  Fst :: Exp (a :*: b) -> Exp a
  Snd :: Exp (a :*: b) -> Exp b
  Fal :: Exp Bool
  Tru :: Exp Bool
  Cnd :: Exp Bool -> Exp a -> Exp a -> Exp a
  InL :: Exp a -> Exp (a :+: b)
  InR :: Exp b -> Exp (a :+: b)
  Cas :: Exp (a :+: b) -> Exp (a -> c) -> Exp (b -> c) -> Exp c
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

      Fal       -> False
      Tru       -> True
      Cnd l m n -> if eval l then eval m else eval n

      InL m     -> Left  (eval m)
      InR n     -> Right (eval n)
      Cas l m n -> case eval l of
                     Left  x -> (eval m) x
                     Right y -> (eval n) y

      Val v     -> v
      Var _     -> error "Impossible!"

type family InT a
type instance InT (Exp a)      = a
type instance InT (a -> b)     = InT a -> InT b
type instance InT (a :*: b)    = InT a :*: InT b
type instance InT Bool         = Bool
type instance InT (a :+: b)    = InT a :+: InT b
type instance InT (Cont Exp a) = InT a

class Reify a where
  reify :: a -> Exp (InT a)
instance Reify (Exp a) where
  reify = id
instance (Reflect a , Reify b) => Reify (a -> b) where
  reify u = Abs (\ x -> reset (reify <$> (u <$> (reflect x))))
instance (Reify a , Reify b) => Reify (a :*: b) where
  reify u = Tup (reify (fst u)) (reify (snd u))
instance Reify Bool where
  reify u = if u then Tru else Fal
instance (Reify a, Reify b) => Reify (a :+: b) where
  reify u = case u of
              Left  x -> InL (reify x)
              Right y -> InR (reify y)
instance Reify a => Reify (Cont Exp a) where
  reify u = reset (fmap reify u)


class Reflect a where
  reflect :: Exp (InT a) -> Cont Exp a
instance Reflect (Exp a) where
  reflect   = return
instance (Reify a , Reflect b) => Reflect (a -> Cont Exp b) where
  reflect l = return (\ x -> reflect (App l (reify x)))
instance (Reflect a , Reflect b) => Reflect (a :*: b) where
  reflect l = ((,)) <$> reflect (Fst l) <*> reflect (Snd l)
instance Reflect Bool where
  reflect l = shift (\ k -> Cnd l (k True) (k False))
instance (Reflect a , Reflect b) => Reflect (a :+: b) where
  reflect l = shift (\ k ->
              Cas l
              (Abs (\ x -> reset ((k . Left)  <$> reflect x)))
              (Abs (\ y -> reset ((k . Right) <$> reflect y))))


-- Continuation Monad
---------------------
-- begin

data Cont c b where
  Cont :: (forall a. (b -> c a) -> c a) -> Cont c b

runCont :: Cont c b -> (forall a. (b -> c a) -> c a)
runCont (Cont k) = k

shift :: (forall a. (b -> c a) -> c a) -> Cont c b
shift k = Cont k

reset :: Cont trm (trm c) -> trm c
reset m = runCont m id

instance Monad (Cont trm) where
  return x = Cont (\ k -> k x)
  m >>= f  = Cont (\ k -> runCont m (\ x -> runCont (f x) k))

instance Functor (Cont trm) where
  fmap  = liftA

instance Applicative (Cont trm) where
  pure  = return
  (<*>) = ap

-- end
------
type a ~> b = a -> Cont Exp b

ex1 :: Exp (Bool -> Bool)
ex1 = reify (\ x -> if x then False else True)

ex2 :: Exp (Bool -> Bool)
ex2 = reify ((\ x -> x ) :: Bool -> Bool)

ex3 :: Exp ((Int -> Bool) :*: Int -> Int)
ex3 = reify ((\ (f , x) -> do c <- f x
                              return (if c then Lit 0 else x))
             :: (Exp Int ~> Bool :*: Exp Int) ~> Exp Int)

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

showExpM Fal         = return "Fal"
showExpM Tru         = return "Tru"
showExpM (Cnd l m n) = showExp3 "Cnd" l m n

showExpM (InL m)     = showExp1 "InL" m
showExpM (InR n)     = showExp1 "InR" n
showExpM (Cas l m n) = showExp3 "Cas" l m n

showExpM (Val _)     = return ("Val _")
showExpM (Var s)     = return s

showExpFM :: (Exp a -> Exp b) -> State Int String
showExpFM f = do i <- get
                 put (i + 1)
                 let x = "x" ++ show i
                 s <- showExpM (f (Var x))
                 return ("\\ " ++ x ++ " -> (" ++ s ++ ")")
