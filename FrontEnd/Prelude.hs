{-# OPTIONS_GHC -Wall #-}
module FrontEnd.Prelude where

import FrontEnd.Core

notE :: Exp Bool -> Exp Bool
notE x   = x ? (FalseE , TrueE)

andE :: Exp Bool -> Exp Bool -> Exp Bool
andE x y = x ? (y , FalseE)

orE :: Exp Bool -> Exp Bool -> Exp Bool
orE  x y = x ? (TrueE , y)

(/=.) :: Exp Word32 -> Exp Word32 -> Exp Bool
x /=. y = notE (x ==. y)

(>.) :: Exp Word32 -> Exp Word32 -> Exp Bool
x >. y = notE (orE (x <. y) (x ==. y))

(<=.) :: Exp Word32 -> Exp Word32 -> Exp Bool
x <=. y = orE (x <. y) (x ==. y)

(>=.) :: Exp Word32 -> Exp Word32 -> Exp Bool
x >=. y = notE (x <. y)

minE :: Exp Word32 -> Exp Word32 -> Exp Word32
minE x y = (x <. y) ? (x , y)

(...) :: Exp Word32 -> Exp Word32 -> Vec (Exp Word32)
m ... n = Vec ((n <. m) ?
               (0 ,
                n - m + 1))
              (+ m)

permute :: (Exp Word32 -> Exp Word32 -> Exp Word32) -> Vec a -> Vec a
permute  f (Vec n g) = Vec n (\ i -> g (f n i))

reverseE :: Vec t -> Vec t
reverseE = permute (\ l i -> l - 1 - i)

zipWithE :: (a -> b -> c) -> Vec a -> Vec b -> Vec c
zipWithE f (Vec l1 g1) (Vec l2 g2) =
          Vec (minE l1 l2)
              (\ i -> f (g1 i) (g2 i))

replicate :: Exp Word32 -> a -> Vec a
replicate n x = Vec n (\ _i -> x)

append :: ExpLike a => Vec a -> Vec a -> Vec a
append (Vec l1 f1) (Vec l2 f2) =
          Vec (l1 + l2)
              (\ i -> (i <. l1) ?
                      (f1 i ,
                       f2 i))
