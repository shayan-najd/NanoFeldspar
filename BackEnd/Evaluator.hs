{-# OPTIONS_GHC -Wall -fno-warn-orphans  #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module BackEnd.Evaluator (evaluate) where

import IL

evaluate :: Prg a -> a
evaluate =  evlPrg

evlPrg :: Prg a -> a
evlPrg (Exp m) = evlExp m
evlPrg (Fun l) = evlPrg . l . unEvlExp

evlExp :: Type a => Exp a -> a
evlExp ee = case ee of
  LitI i     -> i
  Add  m n   -> evlExp m   +   evlExp n
  Sub  m n   -> evlExp m   -   evlExp n
  Mul  m n   -> evlExp m   *   evlExp n
  Div  m n   -> evlExp m `div` evlExp n
  Mod  m n   -> evlExp m `mod` evlExp n
  Eql  m n   -> evlExp m  ==   evlExp n
  Ltd  m n   -> evlExp m  <    evlExp n
  Sqrt m     -> sqrtW (evlExp m)
  LitB b     -> b
  Cnd  l m n -> if evlExp l then evlExp m else evlExp n
  Tup  m n   -> (evlExp m , evlExp n)
  Fst  l     -> fst  (evlExp l)
  Snd  l     -> snd  (evlExp l)
  Ary  m n   -> ary  (evlExp m) (evlExpF n)
  Len  l     -> len  (evlExp l)
  Ind  l m   -> evlExp l ! evlExp m
  Var _      -> error "Impossible!"

evlExpF :: (Type a, Type b) => (Exp a -> Exp b) -> (a -> b)
evlExpF n = evlExp . n . unEvlExp

unEvlExp :: forall a. Type a => a -> Exp a
unEvlExp v = case (typ :: STyp a) of
 SWrd    -> LitI v
 SBol    -> LitB v
 STup {} -> Tup  (unEvlExp (fst v)) (unEvlExp  (snd v))
 SAry {} -> Ary  (unEvlExp (len v)) (unEvlExpF (v !))

unEvlExpF :: (Type a , Type b) => (a -> b) -> (Exp a -> Exp b)
unEvlExpF f = unEvlExp . f . evlExp

ary :: Word32 -> (Word32 -> a) -> Array Word32 a
ary l f = fmap f (listArray (0 , l - 1) [0 .. l - 1])

len :: Array Word32 e -> Word32
len = (1 +) . uncurry (flip (-)) . bounds

sqrtW :: Word32 -> Word32
sqrtW = (round :: Float -> Word32) .
        (sqrt :: Float -> Float) .
        fromIntegral
