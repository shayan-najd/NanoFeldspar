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
module BackEnd.Compiler (compile) where

import Data.List (nub)
import Control.Monad.State

import BackEnd.C
import           IL (Word32,Prg(..),STyp(..),Type(..),typOf)
import qualified IL
import BackEnd.CodeGeneration (pretty)

type CompileMonad a = State (Word32,[Var],[Var]) a

newName :: CompileMonad String
newName = do
 (i , ps , vs) <- get
 put (i+1 , ps , vs)
 return ("v" ++ (show i))

addParam :: Var -> CompileMonad ()
addParam p = do
  (i , ps , vs) <- get
  put (i , p:ps , vs)

addVar :: Var -> CompileMonad ()
addVar v = do
  (i , ps , vs) <- get
  put (i , ps , v:vs)

runCompileMonad :: Typ -> CompileMonad (Exp , [Stmt]) -> Word32 -> Func
runCompileMonad ty m i = let ((e,stmts),(_,ps,vs)) = runState m (i,[],[])
                          in Func ty "func" ps
                                  ([Declare (v , t) | (v , t) <- vs] ++
                                   stmts ++ [Return e])

compileExp :: forall t. (Type t) => IL.Exp t -> CompileMonad (Exp , [Stmt])
compileExp ee  =
 do let t' = cnvTyp (typ :: STyp t)
    case ee of
      IL.Cnd l m n  -> do (el , sl) <- compileExp l
                          (em , sm) <- compileExp m
                          (en , sn) <- compileExp n
                          v <- newName
                          addVar (v , t')
                          return (Var v
                                 , sl ++
                                   [If el
                                    (sm ++ [Assign v em])
                                    (sn ++ [Assign v en])])
      IL.Ary l f    -> do xl <- newName
                          addVar (xl , TWrd)
                          xa <- newName
                          addVar (xa , t')
                          xi <- newName
                          addVar (xi , TWrd)
                          (el , sl) <- compileExp l
                          (ef , sf) <- compileExp (f (IL.Var xi))
                          return ( Var xa
                                 , sl ++
                                   [ Assign xl el
                                   , Assign xa (newAry t' (Var xl))
                                   , Assign xi (Wrd 0)
                                   , Whl (App "ltd" [Var xi,Var xl])
                                         (sf ++
                                          [ Assign xa (App "setAry"
                                                       [Var xa, Var xi, ef])
                                          , Assign xi (App "add"
                                                       [Var xi, Wrd 1])])])
      IL.Var  x     -> cmp0 (Var x)
      IL.LitI i     -> cmp0 (Wrd i)
      IL.LitB True  -> cmp0 (Var "true")
      IL.LitB False -> cmp0 (Var "false")
      IL.Tup m n    -> cmp2 (newTup t')  m n
      IL.Fst l      -> cmp1 (App "fst")  l
      IL.Snd l      -> cmp1 (App "snd")  l
      IL.Len l      -> cmp1 (App "len")  l
      IL.Ind l m    -> cmp2 (App "ind")  l m
      IL.Add m n    -> cmp2 (App "add")  m n
      IL.Sub m n    -> cmp2 (App "sub")  m n
      IL.Mul m n    -> cmp2 (App "mul")  m n
      IL.Div m n    -> cmp2 (App "div")  m n
      IL.Mod m n    -> cmp2 (App "mod")  m n
      IL.Sqrt m     -> cmp1 (App "sqrt") m
      IL.Eql m n    -> cmp2 (App "eql")  m n
      IL.Ltd m n    -> cmp2 (App "ltd")  m n

newTup :: Typ -> [Exp]-> Exp
newTup (TTup tf ts) es = App "newTup"
                              (Var (show (pretty tf) ++ show (pretty ts)) : es)
newTup _ _              = error "Impossible!"

newAry :: Typ -> Exp -> Exp
newAry (TAry t) e  = App "newAry" [Var (show (pretty t)) , e]
newAry _        _  = error "Impossible!"

cmp0 :: Exp -> CompileMonad (Exp , [Stmt])
cmp0 e = return (e , [])

cmp1 :: (Type a) =>
        ([Exp] -> Exp) -> IL.Exp a -> CompileMonad (Exp , [Stmt])
cmp1 f m = do (eM , sM) <- compileExp m
              return (f [eM], sM)

cmp2 :: (Type a, Type b) =>
        ([Exp] -> Exp) -> IL.Exp a -> IL.Exp b -> CompileMonad (Exp , [Stmt])
cmp2 f m n = do (eM , sM) <- compileExp m
                (eN , sN) <- compileExp n
                return (f [eM, eN], sM ++ sN)

compilePrg :: IL.Prg c -> CompileMonad (Exp , [Stmt])
compilePrg (IL.Exp m) = compileExp m
compilePrg (IL.Fun (ef :: IL.Exp a -> IL.Prg b)) =
  do v  <- newName
     addParam (v , cnvTyp (typ :: STyp a))
     compilePrg (ef (IL.Var v))

collectTypesExp :: forall a. Type a => IL.Exp a -> [Typ]
collectTypesExp ee = let t  = typ :: STyp a in
                     cnvTyp t : (
     case ee of
      IL.LitI _x0 -> ([] ++ (const [] _x0))
      IL.Add _x0 _x1 -> (([] ++ (collectTypesExp _x0)) ++ (collectTypesExp _x1))
      IL.Sub _x0 _x1
        -> (([] ++ (collectTypesExp _x0)) ++ (collectTypesExp _x1))
      IL.Mul _x0 _x1
        -> (([] ++ (collectTypesExp _x0)) ++ (collectTypesExp _x1))
      IL.Div _x0 _x1
        -> (([] ++ (collectTypesExp _x0)) ++ (collectTypesExp _x1))
      IL.Mod _x0 _x1
        -> (([] ++ (collectTypesExp _x0)) ++ (collectTypesExp _x1))
      IL.Sqrt _x0 -> ([] ++ (collectTypesExp _x0))
      IL.Eql _x0 _x1
        -> (([] ++ (collectTypesExp _x0)) ++ (collectTypesExp _x1))
      IL.Ltd _x0 _x1
        -> (([] ++ (collectTypesExp _x0)) ++ (collectTypesExp _x1))
      IL.LitB _x0 -> ([] ++ (const [] _x0))
      IL.Cnd _x0 _x1 _x2
        -> ((([] ++ (collectTypesExp _x0)) ++ (collectTypesExp _x1))
            ++ (collectTypesExp _x2))
      IL.Tup _x0 _x1
        -> (([] ++ (collectTypesExp _x0)) ++ (collectTypesExp _x1))
      IL.Fst _x0 -> ([] ++ (collectTypesExp _x0))
      IL.Snd _x0 -> ([] ++ (collectTypesExp _x0))
      IL.Ary _x0 _x1
        -> (([] ++ (collectTypesExp _x0)) ++ (collectTypes
                                              (Fun (Exp . _x1))))
      IL.Len _x0 -> ([] ++ (collectTypesExp _x0))
      IL.Ind _x0 _x1
        -> (([] ++ (collectTypesExp _x0)) ++ (collectTypesExp _x1))
      IL.Var _x0 -> ([] ++ (const [] _x0)))

collectTypes :: forall c. Prg c -> [Typ]
collectTypes (Exp ee) = collectTypesExp ee
collectTypes (Fun (f :: IL.Exp a -> Prg b)) =
  let t  = typ :: STyp a in
  cnvTyp t : collectTypes (f (IL.Var "_dummy"))

genStructs :: [Typ] -> String
genStructs ts = concat (
               ["typedef struct {Wrd size; "++s++"* elems;} Ary"++s++";\n"
               | TAry a <- ts,
                 a /= TWrd,
                 let s = show (pretty a)] ++
               ["typedef struct {"++sa++" fst; "++sb++" snd;} Tup"++sa++sb++";\n"
               | TTup a b <- ts,
                 let sa = show (pretty a),
                 let sb = show (pretty b)])

scompileWith :: Bool -> [Var] -> STyp t -> Word32 -> Prg a -> String
scompileWith b vs t i e = let c  = runCompileMonad (cnvTyp t)
                                     (do mapM_ addParam vs
                                         compilePrg e) i
                          in  (if b then "#include \"header.h\"\n\n" else "") ++
                              genStructs (nub (collectTypes e)) ++ "\n" ++
                              ((show . pretty) c)

compile :: Prg a -> String
compile p = scompileWith False [] (result p) 0 p

type family Result a where
  Result (a -> b) = Result b
  Result a        = a

result :: Prg a -> STyp (Result a)
result (Exp m) = let t = IL.typOf m in
  case t of
    SWrd    -> t
    SBol    -> t
    STup {} -> t
    SAry {} -> t
result (Fun f) = result (f (IL.Var "dummy"))

cnvTyp :: STyp a -> Typ
cnvTyp  tt = case tt of
    SWrd       -> TWrd
    SBol       -> TBol
    STup tf ts -> TTup (cnvTyp tf) (cnvTyp ts)
    SAry ta    -> TAry (cnvTyp ta)
