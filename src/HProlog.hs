module HProlog (
  Pred(..), Expr(..), Rule(..)
, unify, replSubVars, applySubExpr, applySubPred, query
) where

import Data.Monoid ((<>))
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Control.Monad
import Control.Monad.State

import Debug.Trace

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

unify         = unify_ True
unifyNoOccurs = unify_ False

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

getExprVars :: Expr -> [T.Text]
getExprVars (C _)      = []
getExprVars (V var)    = [var]
getExprVars (F _ args) = concatMap getExprVars args

getPredVars :: Pred -> [T.Text]
getPredVars (P _ args) = concatMap getExprVars args


-- rule: consequent, list of antecedents
data Rule = Rule Pred [Pred] deriving (Show)

query :: [Rule] -> Pred -> [Sub]
query kb goal =
  let vars = getPredVars goal in
  let subs = evalState (backwardChainOr goal []) M.empty in
  map (filter (\(v,_) -> v `elem` vars)) subs
  where backwardChainOr :: Pred -> Sub -> State (M.Map T.Text Int) [Sub]
        backwardChainOr goal sub = do
          let goalRules = getGoalRules goal
          subs' <- forM goalRules $ \rule -> do
            Rule rhs lhs <- getFreshVars rule
            backwardChainAnd lhs (unifyNoOccurs rhs goal sub)
          return (concat subs')

        backwardChainAnd :: [Pred] -> Maybe Sub -> State (M.Map T.Text Int) [Sub]
        backwardChainAnd goals msub
          | Nothing <- msub                 = return []
          | Just sub <- msub, [] <- goals   = return [sub]
          | Just sub <- msub, g:gs <- goals = do
            subs' <- backwardChainOr (applySubPred sub g) sub
            subs'' <- forM subs' $ \sub' -> backwardChainAnd gs (Just sub')
            return (concat subs'')

        getGoalRules :: Pred -> [Rule]
        getGoalRules (P pname args) =
          let arity = length args in
          filter (isGoalRule pname arity) kb
          where isGoalRule pname arity (Rule (P pname' args') _) =
                  pname == pname' && arity == length args'

        getFreshVars :: Rule -> State (M.Map T.Text Int) Rule
        getFreshVars (Rule rhs lhs) = do
          let vars = L.nub $ (getPredVars rhs) ++ (concatMap getPredVars lhs)
          sub <- forM vars $ \var -> do
            num <- getVarNumber var
            return (var, V (var <> (T.pack $ show num)))
          let rhs' = applySubPred sub rhs
          let lhs' = map (applySubPred sub) lhs
          return (Rule rhs' lhs')
            
        getVarNumber :: T.Text -> State (M.Map T.Text Int) Int
        getVarNumber var = do
          names <- get
          case M.lookup var names of
            Nothing -> do
              put $ M.insert var 1 names
              return 0
            Just n -> do
              let n' = n + 1
              put $ M.insert var n' names
              return n
                
