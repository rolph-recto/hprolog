module HProlog.Query (
  unify, replSubVars, applySubExpr, applySubPred,
  removeUnseenVars, getPredVars, query
) where

import Data.Monoid ((<>))
import Data.Maybe
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Control.Monad
import Control.Monad.State

import HProlog.Expr

import Debug.Trace

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
          | F fx xargs <- x, F fy yargs <- y, fx == fy,
            length xargs == length yargs =
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
replSubVars sub = map replSubVar sub
  where replSubVar (v,vs) =
          let applyExpr = applySubExpr sub vs in (v, applyExpr)

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

type QueryM a = StateT (M.Map T.Text Int) [] a

removeUnseenVars :: [T.Text] -> Sub -> Sub
removeUnseenVars vars [] = []
removeUnseenVars vars ((v,!vs):ss)
  | v `elem` vars = (v,vs):(removeUnseenVars vars ss)
  | otherwise     = removeUnseenVars vars ss

query :: [Rule] -> Pred -> [Sub]
query kb goal = 
  let subs    = evalStateT (backwardChainOr goal []) M.empty in
  -- let vars    = getPredVars goal in
  -- let subs'   = map (replSubVars . toposortSub) subs in
  -- map (removeUnseenVars vars) subs'
  subs
  where backwardChainOr :: Pred -> Sub -> QueryM Sub
        backwardChainOr goal sub = do
          let goalRules = getGoalRules goal
          goalRule <- getGoalRules goal
          Rule head body <- getFreshVars goalRule
          -- if the head of the rule fails to unify with the 
          -- goal, then we kill this branch of the search path.
          let sub' = unifyNoOccurs head goal sub
          backwardChainAnd body sub'

        backwardChainAnd :: [Pred] -> Maybe Sub -> QueryM Sub
        backwardChainAnd goals msub
          -- if m is a MonadPlus, then StateM s m a is a MonadPlus also
          -- in this case, the mzero for list is [], which effectively
          -- kills this branch of the search path.
          | Nothing <- msub                 = do
            mzero

          | Just sub <- msub, [] <- goals   = do
            return sub

          | Just sub <- msub, g:gs <- goals = do
            -- we try to find a proof for the head
            let g' = applySubPred sub g
            sub' <- backwardChainOr g' sub
            -- then we try to find proofs for the rest of preds in the tail
            backwardChainAnd gs (Just sub')

        -- get rules whose head *might* unify with the goal
        getGoalRules :: Pred -> QueryM Rule
        getGoalRules (P pname args) =
          let arity = length args in
          let rules = filter (isGoalRule pname arity) kb in
          StateT (\s -> map (,s) rules)
          where isGoalRule pname arity (Rule (P pname' args') _) =
                  pname == pname' && arity == length args'
        
        -- rewrite rule so it has fresh variables
        getFreshVars :: Rule -> QueryM Rule
        getFreshVars (Rule head body) = do
          let vars = L.nub $ (getPredVars head) ++ (concatMap getPredVars body)
          sub <- forM vars $ \var -> do
            num <- getVarNumber var
            return (var, V (var <> "#" <> (T.pack $ show num)))
          let head' = applySubPred sub head
          let body' = map (applySubPred sub) body
          return (Rule head' body')
            
        getVarNumber :: T.Text -> QueryM Int
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

        -- topologically sort subs based on the occurences
        -- of vars in the substitution
        toposortSub :: Sub -> Sub
        toposortSub s = toposortSub_ subGraph s
          where toposortSub_ g [] = []
                toposortSub_ g s  =
                  let (x:xs) = L.sortBy (cmpInEdges g) s in
                  x:(toposortSub_ (filter (\(n,_) -> n /= fst x) g) xs)
                makeEdges (name, F _ args) = concatMap (curry makeEdges name) args
                makeEdges (name, V vname)  = [(name, vname)]
                makeEdges (name, C _)      = []
                subGraph = map makeEdges s >>= id
                cmpInEdges g (n1,_) (n2,_) = inEdges g n2 `compare` inEdges g n1
                inEdges g n = length $ filter (\(_,n') -> n == n') g
