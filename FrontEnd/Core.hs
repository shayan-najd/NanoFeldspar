{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE DeriveFunctor               #-}
{-# LANGUAGE KindSignatures              #-}
{-# LANGUAGE GADTs                       #-}
{-# LANGUAGE TypeFamilies                #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE FlexibleContexts            #-}
{-# LANGUAGE PatternSynonyms             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE ScopedTypeVariables         #-}
{-# LANGUAGE EmptyCase                   #-}
module FrontEnd.Core
  (module IL,ExpLike(..),PrgLike(..),InT,
   divE,modE,sqrtE,(==.),(<.),pattern TrueE, pattern FalseE,(?),
   Vec(..), evaluate,compile,
   makeIP,makeIPAt,expand) where

import System.Process
import System.Random
import qualified Language.Haskell.TH.Syntax as TH

import IL
import qualified BackEnd.Compiler  as BackEnd (compile)
import qualified BackEnd.Evaluator as BackEnd (evaluate)

instance Num (Exp Word32) where
  fromInteger = LitI . fromInteger
  (+)         = Add
  (-)         = Sub
  (*)         = Mul
  abs    x    = x
  signum x    = 1

divE :: Exp Word32 -> Exp Word32 -> Exp Word32
divE = Div

modE :: Exp Word32 -> Exp Word32 -> Exp Word32
modE = Mod

sqrtE :: Exp Word32 -> Exp Word32
sqrtE = Sqrt

infix 4 ==.
(==.) :: Exp Word32 -> Exp Word32 -> Exp Bool
(==.) = Eql

infix 4 <.
(<.) :: Exp Word32 -> Exp Word32 -> Exp Bool
(<.) = Ltd

compile :: PrgLike a => a -> String
compile = BackEnd.compile . toPrg

evaluate :: PrgLike a => a -> (InT a)
evaluate = BackEnd.evaluate . toPrg

type family InT a
class PrgLike a where
  toPrg  :: a -> Prg (InT a)
  frmPrg :: Prg (InT a) -> a
class Type (InT a) => ExpLike a where
  toExp  :: a -> Exp (InT a)
  frmExp :: Exp (InT a) -> a
type instance InT (Prg a) = a
instance PrgLike (Prg a) where
  toPrg  = id
  frmPrg = id
type instance InT (Exp a) = a
instance Type a => ExpLike (Exp a) where
  toExp  = id
  frmExp = id
instance Type a => PrgLike (Exp a) where
  toPrg = Exp
  frmPrg (Exp m) = m
  frmPrg (Fun _) = case typ :: STyp a of {}
                   -- impossible
type instance InT (a -> b) = InT a -> InT b
instance (ExpLike a, PrgLike b) => PrgLike (a -> b) where
  toPrg  f       = Fun (toPrg . f . frmExp)
  frmPrg (Exp m) = case typOf m of {}
                   -- impossible
  frmPrg (Fun f) = frmPrg . f . toExp
type instance InT (a , b) = (InT a , InT b)
instance (ExpLike a , ExpLike b) => PrgLike (a , b) where
  toPrg  u       = Exp (toExp u)
  frmPrg (Exp l) = frmExp l
instance (ExpLike a , ExpLike b) => ExpLike (a , b) where
  toExp  u = Tup (toExp (fst u)) (toExp (snd u))
  frmExp l = (frmExp (Fst l) , frmExp (Snd l))

data Vec a
  = Vec { len :: Exp Word32,
          ind :: Exp Word32 -> a}
    deriving Functor

type instance InT (Vec a) = Ary (InT a)
instance (ExpLike a) => PrgLike (Vec a) where
  toPrg  u       = Exp (toExp u)
  frmPrg (Exp l) = frmExp l
instance ExpLike a => ExpLike (Vec a) where
  toExp  u = Ary (len u) (\ x -> toExp  (ind u x))
  frmExp l = Vec (Len l) (\ x -> frmExp (Ind l x))

{-
type instance InT Bool = Bool
instance PrgLike Bool where
  toPrg  u       = Exp (toExp u)
  frmPrg (Exp l) = frmExp l
instance ExpLike Bool where
  toExp  u = if u then LitB True else LitB False
  frmExp l = Cnd l ? ?
-}

pattern TrueE :: () => a ~ Bool => Exp a
pattern TrueE = LitB True

pattern FalseE :: () => a ~ Bool => Exp a
pattern FalseE = LitB False

(?) :: ExpLike a => Exp Bool -> (a , a) -> a
l ? (v , w) = frmExp (Cnd l (toExp v) (toExp w))

makeIP :: String -> String -> IO ()
makeIP = makeIPAt "./Examples/C/"

makeIPAt :: String -> String -> String -> IO ()
makeIPAt path c name  = do
  let fileContent =
       $(do f1 <- TH.runIO (readFile "BackEnd/ppm.h")
            f2 <- TH.runIO (readFile "BackEnd/header.h")
            TH.lift (f1 ++"\n" ++ f2))
        ++ "\n" ++
        c
        ++
        "\nint main(int argc, char *argv[])"++
        "\n{"++
        "\n  Image   imgIn = readImage(argv[1]);"++
        "\n  AryWrd aryIn = newAry(Wrd,size(imgIn)); "++
        "\n  for (unsigned int i = 0; i < size(imgIn); i++)"++
        "\n    aryIn = setAry(aryIn , i , imgIn.data[i]);"++
        "\n  AryWrd aryOut;"++
        "\n  aryOut = func(aryIn);"++
        "\n  unsigned int t;"++
        "\n  sscanf(argv[3],\"%d\",&t);" ++
        "\n  Image imgOut = {.sizeX = imgIn.sizeX, "++
        "\n                  .sizeY = imgIn.sizeY,"++
        "\n                  .type  = t,"++
        "\n                  .data  = malloc(len(aryOut) * (t == 3 ? 3 : 1) * "
        ++ "sizeof(unsigned int))}; "++
        "\n  for(unsigned int i = 0; i < size(imgOut); i++)"++
        "\n    imgOut.data[i] = ind(aryOut , i);"++
        "\n  writeImage (argv[2] , imgOut);"++
        "\n  return 0;"++
        "\n}"
  writeFile (path ++ "/" ++ name++".c") fileContent
  _ <- runCommand ("gcc -o " ++ path ++ "/" ++ name ++" " ++ path ++ "/"
                   ++ name ++ ".c -lm -std=c99")
  return ()

expand :: String -> IO ()
expand code = do let c = $(do f1 <- TH.runIO (readFile "BackEnd/ppm.h")
                              f2 <- TH.runIO (readFile "BackEnd/header.h")
                              TH.lift (f1 ++"\n" ++ f2)) ++ "\n" ++ code
                 i <- randomIO :: IO Word32
                 let file = "Temp" ++ show i ++ ".c"
                 writeFile file c
                 _ <- runCommand ("gcc -E " ++ file)
                 return ()
