module HProlog (
  Pred(..), Expr(..)
, unify, replSubVars, applySubExpr, applySubPred
) where

import Data.Monoid
import qualified Data.List as L
import qualified Data.Text as T
import Control.Monad

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

unify_ :: Bool -> Pred -> Pred -> Sub -> Maybe Sub
unify_ occursCheck x y s
  | P px xargs <- x, P py yargs <- y, px == py, length xargs == length yargs =
    foldr (unifyArg unifyExpr) (Just s) (zip xargs yargs)
  | otherwise = Nothing

  where unifyExpr :: Expr -> Expr -> Maybe Sub -> Maybe Sub
        unifyExpr x y s
          | Nothing <- s  = Nothing
          | x == y        = s
          | V vx <- x     = unifyVar x y s
          | V vy <- y     = unifyVar y x s
          -- unify function names
          | F fx xargs <- x, F fy yargs <- y, fx == fy, length xargs == length yargs =
            foldr (unifyArg unifyExpr) s (zip xargs yargs)
          | otherwise     = Nothing

        unifyVar :: Expr -> Expr -> Maybe Sub -> Maybe Sub
        unifyVar var@(V vname) x (Just s)
          | Just val <- lookup vname s                = unifyExpr val x (Just s)
          | V xname <- x, Just val <- lookup xname s  = unifyExpr var val (Just s)
          | occursCheck, vname `occurs` x             = Nothing
          | otherwise                                 = Just $ (vname,x):s
        unifyVar _ _ _                                = Nothing

        occurs :: T.Text -> Expr -> Bool
        occurs vname x
          | V xname <- x, vname == xname  = True
          | C _ <- x                      = False
          | F _ args <- x                 = any (vname `occurs`) args
          | otherwise                     = False

        unifyArg :: (a -> a -> Maybe Sub -> Maybe Sub) -> (a,a) -> (Maybe Sub) -> Maybe Sub
        unifyArg f (xarg,yarg) acc = acc >>= \accSub -> f xarg yarg (Just accSub)

-- apply a substitution to an expression
applySubExpr :: Sub -> Expr -> Expr
applySubExpr sub (C const)      = C const
applySubExpr sub (F fname args) = F fname (map (applySubExpr sub) args)
applySubExpr sub (V var)        = maybe (V var) id (lookup var sub)

-- replace variables in substitutions
-- e.g. [y/father(x), x/jane] becomes [y/father(jane), x/jane]
replSubVars :: Sub -> Sub
replSubVars sub = map (\(v,vs) -> (v,applySubExpr sub vs)) sub

-- apply a substitution to a predicate
applySubPred :: Sub -> Pred -> Pred
applySubPred s (P pname args) =
  let s' = replSubVars s in P pname (map (applySubExpr s') args)

unify         = unify_ True
unifyNoOccurs = unify_ False

-- rule: consequent, list of antecedents
data Rule = Rule Pred [Pred]

