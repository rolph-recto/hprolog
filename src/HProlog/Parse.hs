module HProlog.Parse (
  parseAssert, parseFile, parseQuery
) where

import Data.Monoid ((<>))
import Text.Parsec
import qualified Data.Text as T
import qualified Data.List as L
import Data.String.Utils as S

import HProlog.Expr
import Debug.Trace

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

removeComments :: String -> String
removeComments =
  -- let's make this unreadable using
  -- pointfree notation and the reader monad!
  L.unlines . filter f . map S.lstrip . L.lines
  where f = do
          x <- ((> 0) . length)
          y <- ((/= ';') . head)
          return (x && y)

removeWhitespace :: String -> String
removeWhitespace = L.intercalate "" . L.words

preprocessInput :: String -> String
preprocessInput = removeWhitespace . removeComments

parseAssert :: String-> Either ParseError Rule
parseAssert text =
  let f = parseStmt $ (try parseRule) <|> parseFact in
  parse f "" $ preprocessInput text

parseFile :: String -> Either ParseError [Rule]
parseFile text =
  let f = many $ parseStmt $ (try parseRule) <|> parseFact in
  parse f "" $ preprocessInput text

parseQuery :: String-> Either ParseError Pred
parseQuery text =
  parse (parseStmt parsePred) "" $ preprocessInput text
