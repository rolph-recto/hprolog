import Data.Text.IO as T
import HProlog

main = do
  let x = P "Knows" [V "y", C "john", V "x"]
  let y = P "Knows" [F "father" [F "father" [V "x"]], C "john", F "mother" [C "jane"]]
  print $ unify y x [] >>= return . replSubVars
