import qualified Data.List as L
import qualified Data.Char as C
import qualified Data.Text as T
import Control.Monad

import HProlog.Expr
import HProlog.Query
import HProlog.Parse

main = do
  let r1 = Rule (P "descendant" [V "X", V "Y"]) [P "childOf" [V "X", V "Y"]]
  let r2 = Rule (P "descendant" [V "X", V "Y"]) [P "childOf" [V "X", V "Z"], P "descendant" [V "Z", V "Y"]]
  let f1 = Rule (P "childOf" [C "mary", C "magdalene"]) []
  let f2 = Rule (P "childOf" [C "magdalene", C "martha"]) []
  putStr "> "
  queryStr <- getLine
  case parseQuery queryStr of
    Left err -> print err
    Right q -> do
      let result = query [r1,r2,f1,f2] q
      putStrLn $ "Query: " ++ (show q)
      if length result > 0
      then loop q $ filter (\s -> length s > 0) result
      else putStrLn "No."

  where loop q []     = putStrLn "Yes."
        loop q (s:ss) = do
          putStrLn (show q)
          putStrLn $ showSub $ replSubVars s
          putStrLn "Next? (Y/N)"
          input <- getLine
          if map C.toLower input == "y"
          then loop q ss
          else return ()
        showSub s =
          let showSubst (v,e) = (T.unpack v) ++ " => " ++ (show e) in
          L.intercalate ", " $ map showSubst s

