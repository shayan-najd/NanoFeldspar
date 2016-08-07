{-# OPTIONS_GHC -Wall            #-}
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
module BackEnd.CodeGeneration where

import Text.PrettyPrint
import qualified Data.List

import BackEnd.C

class Pretty a where
 pretty :: a -> Doc

instance Pretty Exp where
 pretty (Var x)     = text x
 pretty (Wrd i)     = text (show i++"u")
 pretty (App op es) = text op <+> parens (commaCat (fmap pretty es))

instance Pretty Stmt where
 pretty (Whl e ss) = text "while" <+> parens (pretty e)
  $+$ (lbrace
   $+$ nest 2 (vcat (fmap pretty ss))
   $+$ rbrace)
 pretty (If e1 ss1 ss2) = text "if" <+> parens (pretty e1)
  $+$ (lbrace
   $+$ nest 2 (vcat (fmap pretty ss1))
   $+$ rbrace)
  $+$ text "else"
  $+$ (lbrace
   $+$ nest 2 (vcat (fmap pretty ss2))
   $+$ rbrace)
 pretty (Assign v e)      = text v <+> text "=" <+> pretty e <> semi
 pretty (Declare (v , t)) = pretty t <+> text v <> semi
 pretty (Return e)        = text "return" <+> pretty e <> semi

instance Pretty Func where
 pretty (Func ty name vs ss) =
  pretty ty <+> text name
  <+> parens (commaCat (fmap pretty vs) )
  $+$ (lbrace
   $+$ nest 2 (vcat (fmap pretty ss))
   $+$ rbrace)

instance Pretty Var where
 pretty (v,t) = pretty t <+> text v

instance Pretty Typ where
  pretty t = case t of
    TWrd     -> text "Wrd"
    TBol     -> text "Bol"
    TTup a b -> text "Tpl" <> pretty a <> pretty b
    TAry a   -> text "Ary" <> pretty a

commaCat :: [Doc] -> Doc
commaCat ds = foldl1 (<>) (Data.List.intersperse (comma<>space) ds)
