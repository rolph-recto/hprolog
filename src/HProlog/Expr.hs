module HProlog.Expr (
  Pred(..), Expr(..), Rule(..), Sub
) where

import qualified Data.List as L
import qualified Data.Text as T

data Pred = P T.Text [Expr] -- predicates or function names
            deriving (Eq)

data Expr = V T.Text        -- variable
          | C T.Text        -- constant name
          | F T.Text [Expr] -- function names
          deriving (Eq)

instance Show Pred where
  show (P p args) = (T.unpack p) ++ "(" ++ (L.intercalate "," (map show args)) ++ ")"

instance Show Expr where
  show (V vname)  = T.unpack vname
  show (C cname)  = T.unpack cname
  show (F f args) = (T.unpack f) ++ "(" ++ (L.intercalate "," (map show args)) ++ ")"

type Sub = [(T.Text,Expr)]

-- rule: consequent, list of antecedents
data Rule = Rule Pred [Pred] deriving (Show)

