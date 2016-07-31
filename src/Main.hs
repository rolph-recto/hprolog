import qualified Data.List as L
import qualified Data.Char as C
import qualified Data.Text as T
import Control.Monad

import HProlog

main = do
  let r1 = Rule (P "descendant" [V "x", V "y"]) [P "childOf" [V "x", V "y"]]
  let r2 = Rule (P "descendant" [V "x", V "y"]) [P "childOf" [V "x", V "z"], P "descendant" [V "z", V "y"]]
  let f1 = Rule (P "childOf" [C "Mary", C "Magdalene"]) []
  let f2 = Rule (P "childOf" [C "Magdalene", C "Martha"]) []
  let q = P "descendant" [V "x", C "Martha"]
  let result = query [r1,r2,f1,f2] q
  putStrLn $ "Query: " ++ (show q)
  loop result
  where loop []     = putStrLn "Yes."
        loop (s:ss) = do
          putStrLn $ showSub $ replSubVars s
          putStrLn "Next (Y/N)"
          input <- getLine
          if map C.toLower input == "y"
          then loop ss
          else return ()
        showSub s =
          let showSubst (v,e) = (T.unpack v) ++ " => " ++ (show e) in
          L.intercalate ", " $ map showSubst s

