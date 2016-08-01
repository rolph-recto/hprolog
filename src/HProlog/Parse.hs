module HProlog.Parse (
  parseAssert, parseQuery
) where

import Data.Monoid ((<>))
import Text.Parsec
import qualified Data.Text as T
import qualified Data.List as L

import HProlog.Expr

-- name must start with a lowercase character
parseName :: Parsec String () T.Text
parseName = do
  n <- lower
  ns <- many alphaNum
  return $ T.pack $ n:ns

parseConst :: Parsec String () Expr
parseConst = do
  name <- parseName
  return $ C name

parseVar :: Parsec String () Expr
parseVar = do
  v <- upper
  vs <- many alphaNum
  return $ V (T.pack $ v:vs)

parseFunc :: Parsec String () Expr
parseFunc = parseNameWithArgs F

parseExpr :: Parsec String () Expr
parseExpr = (try parseFunc) <|> (try parseConst) <|> parseVar

parseNameWithArgs :: (T.Text -> [Expr] -> a) -> Parsec String () a
parseNameWithArgs f = do
  name <- parseName
  char '('
  args <- sepBy1 parseExpr (char ',')
  char ')'
  return (f name args)

parsePred :: Parsec String () Pred
parsePred = parseNameWithArgs P

parseRule :: Parsec String () Rule
parseRule = do
  head <- parsePred
  string "<="
  body <- sepBy1 parsePred (char ',')
  return $ Rule head body

parseFact :: Parsec String () Rule
parseFact = do
  pred <- parsePred
  return $ Rule pred []

parseStmt :: Parsec String () a -> Parsec String () a
parseStmt f = do
  stmt <- f
  optional $ char '.'
  return stmt

removeWhitespace :: String -> String
removeWhitespace = L.intercalate "" . L.words

parseAssert :: String-> Either ParseError Rule
parseAssert text = parse
                    (parseStmt $ (try parseRule) <|> parseFact)
                    ""
                    (removeWhitespace text)

parseQuery :: String-> Either ParseError Pred
parseQuery text = parse
                    (parseStmt parsePred)
                    ""
                    (removeWhitespace text)
